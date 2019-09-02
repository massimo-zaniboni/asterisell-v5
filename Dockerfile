FROM centos:centos7
MAINTAINER Massimo Zaniboni - massimo.zaniboni@asterisell.com
LABEL Description="This image is used for building Asterisell (https://www.asterisell.com) and installing it on remote hosts."

# From https://bugzilla.redhat.com/show_bug.cgi?id=1213602
RUN touch /var/lib/rpm/*

# -----------------------------------------
# Base and DEVEL packages

RUN yum update -y ; yum -y groupinstall Base ; yum -y install git awk sqlite sqlite-devel rsync

RUN yum groupinstall "Development Tools" -y
RUN yum install -y wget curl libbsd-devel zlib-devel make perl gcc \
                   which glib2 glib2-devel openssl-devel git \
                   java-1.8.0-openjdk \
                   cmake libbsd-devel gmp gmp-devel \
                   cmake Judy Judy-devel \
                   glib2 glib2-devel clips clips-devel clips-libs \
                   gnupg libffi xz tar \
                   vim

# -----------------------------------------
# Install packages for Symphony
#
# NOTE: Asterisell code run on a more recent version of PHP,
# but the old Symphony management files requires an older
# version of PHP (the version shipped with CentOS 7).
# So the management utility uses an old version of PHP,
# while the installed code a more recent.

RUN yum install -y httpd-tools php php-mysqlnd php-pdo \
                   php-cli php-common php-opcache php-bcmath \
                   php-xml php-mbstring php-gd php-fpm \
                   mingw32-iconv gmp lftp

# -----------------------------------------
# Call flow merging

RUN yum groupinstall -y "Development Tools" -y && \
    yum install -y wget curl cmake libbsd-devel gmp-devel gmp zlib-devel \
                   make perl gcc which cmake Judy Judy-devel openssh-server \
                   glib2 glib2-devel clips clips-devel clips-libs \
                   openssl-devel git

# --------------------------------
# Documentation generation tools

RUN cd /opt && wget  https://downloads.asterisell.com/ditac-3_3_1-plus-fop.zip
RUN cd /opt && unzip ditac-3_3_1-plus-fop.zip && mv ditac-3_3_1 ditac && chmod -R ugo+rwx /opt/ditac

RUN yum -y install epel-release && \
    yum -y install python2-pip && \
    pip install --upgrade pip

RUN pip install pelican Markdown typogrify

# ---------------------------------
# Fabric

RUN yum groupinstall -y development && yum install -y epel-release
RUN yum install -y git openssl-devel fabric

# ----------------------------------
# Fabricate

RUN pip install fabricate

# --------------------------------------------------------------
# Groovy and Java
# Needed from some customers for generating customizations.

RUN yum install -y bsdtar java-1.8.0-openjdk-devel && \
    cd /opt && \
    wget https://bintray.com/artifact/download/groovy/maven/apache-groovy-binary-2.5.6.zip && \
    mkdir -p /opt/groovy && cd groovy && bsdtar --strip-components=1 -xvf /opt/apache-groovy-binary-2.5.6.zip && \
    chmod -R ugo+rwx /opt/groovy && cd .. && \
    wget https://services.gradle.org/distributions/gradle-5.4.1-bin.zip && \
    mkdir -p /opt/gradle && cd gradle && bsdtar --strip-components=1 -xvf /opt/gradle-5.4.1-bin.zip && \
    chmod -R ugo+rwx /opt/gradle

# -----------------------------------------
# Haskell compilation tools

RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | tee /etc/yum.repos.d/fpco.repo
RUN yum -y install stack

# NOTE: use an user distinct from root because it is more safe,
# and because it will be the same user invoking the management utility

ARG USER_ID
ARG GROUP_ID

RUN groupadd -g $GROUP_ID user && useradd -u $USER_ID -r -b /home --create-home  -g user user
USER user

ENV PATH="/local/bin:/home/user/.local/bin:/home/user/.cabal/bin:${PATH}"

RUN stack upgrade 

# --------------------------------------------------
# SSH access from the container to Asterisell hosts

RUN cd /home/user && mkdir -p .ssh && cd .ssh && ssh-keygen -b 4096  -t rsa -C "asterisell@example.net" -q -P "" -f id_rsa

# -----------------------------------------
# These actions **must** be the last!!!

RUN touch /home/user/i_am_a_docker_container_for_asterisell_management

RUN echo "PATH=/local/bin:/home/user/.local/bin:/home/user/.cabal/bin:/opt/gradle/bin:/opt/groovy/bin:\$PATH" >> /home/user/.bashrc && \
    echo "export PATH" >> /home/user/.bashrc && \
    echo "export GRADLE_HOME=/opt/gradle" >> /home/user/.bashrc && \
    echo "export GROOVY_HOME=/opt/groovy" >> /home/user/.bashrc

# Use the native FS (but inside a private volume in /var directory),
# because in case of Stack updates, it is a lot faster respect container FS.
VOLUME /home
VOLUME /local
VOLUME /var

# NOTE: expect that `/asterisell` directory is a volume mounted on the local FS containing Asterisell code 
WORKDIR /asterisell
CMD "/bin/bash"
