FROM centos:centos7
MAINTAINER Massimo Zaniboni - massimo.zaniboni@asterisell.com

# Broken sym-link (/var/lock) workaround
RUN mkdir -p /run/lock

# From https://bugzilla.redhat.com/show_bug.cgi?id=1213602
RUN touch /var/lib/rpm/*

# REMI is a repository with last versions of PHP.
# EPEL is a repository with extra packages.
# They are both widely used and tested.
RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm && yum -y install http://rpms.remirepo.net/enterprise/remi-release-7.rpm && yum -y install yum-utils && yum-config-manager --enable remi-php56

# Install standard packages, and update according REMI suggestions.
RUN yum update -y ; yum -y groupinstall Base

# Install Asterisell needed packages.
RUN yum -y install git awk sqlite sqlite-devel rsync openssh-server libXpm libvpx libt1 libexslt libxslt libfreetype apr apr-util mailcap php php-mysqlnd php-pdo php-cli php-common php-opcache php-bcmath php-xml php-mbstring php-gd php-fpm mingw32-iconv gmp lftp php-gd php-xml libxslt freetype gnutls httpd-tools mingw-filesystem-base mingw32-crt mingw32-filesystem nettle php-pear php-pecl-jsonc php-process t1lib trousers

# MariaDB is a community mantained fork of MySQL
ADD MariaDB.repo /etc/yum.repos.d/MariaDB.repo
RUN yum -y install MariaDB-server MariaDB-client MariaDB-devel

# Use NGINX version directly packaged from NGINX web site, because it is a lot more recent than CentOS version.
RUN wget http://nginx.org/keys/nginx_signing.key
RUN rpm --import nginx_signing.key
ADD nginx.repo /etc/yum.repos.d/nginx.repo
RUN yum --nogpgcheck -y install nginx

# Compile and Install RAGEL
RUN yum groupinstall "Development Tools" -y ; yum install -y wget curl cmake libbsd-devel gmp-devel gmp zlib-devel make perl gcc which cmake Judy Judy-devel openssh-server glib2 glib2-devel clips clips-devel clips-libs openssl-devel git
ADD ragel-6.10.tar.gz /root/
RUN cd /root/ragel-6.10 && ./configure && make && make install

#
# Install Haskell Compilation Env
#

RUN curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | tee /etc/yum.repos.d/fpco.repo
RUN yum -y install stack gnupg gmp-devel libffi zlib xz tar git && cd && stack setup && stack install cabal-install

ENV PATH /local/bin:/root/.local/bin:/root/.cabal/bin:$$PATH

# Precompile Haskell application so all Stack packages are downloaded
# and the big compilation environment is shared between multiple
# containers.
RUN mkdir -p /root/rate_engine
COPY rate_engine /root/rate_engine
RUN chown -R root:root /root/rate_engine
RUN cd /root/rate_engine && stack setup
RUN cd /root/rate_engine && stack clean && stack build || :

#
# Settings that can change often
#

# Manage services using supervisord instead of systemd
RUN yum install -y supervisor
ADD supervisord.conf /etc/supervisord.conf

# Configure MariaDB.
# NOTE: this is a temporary setup. It will be replaced by fabric generated setup laters.
ADD server.cnf /etc/my.cnf.d/server.cnf

# Configure php-fpm
# NOTE: the majority of this settings have effect on http sessions, because
# for CLI sessions by default there are no limits
# http://php.net/manual/en/timezones.europe.php"""
RUN sed -i "/cgi\.fix_pathinfo/c\cgi.fix_pathinfo=0" /etc/php.ini && sed -i "/max_execution_time/c\max_execution_time = 180" /etc/php.ini && sed -i "/max_input_time/c\max_input_time = 180" /etc/php.ini && sed -i "/post_max_size/c\post_max_size = 30M" /etc/php.ini &&  sed -i "/upload_max_filesize/c\upload_max_filesize = 30M" /etc/php.ini && sed -i "/opcache.max_accelerated_files=.*/c\opcache.max_accelerated_files=12000" /etc/php.d/10-opcache.ini

# NOTE: this is needed only for Docker and supervisord
RUN sed -i -e "s/;daemonize\s*=\s*yes/daemonize = no/g" /etc/php-fpm.conf && sed -i -e "s/user\s*=.*/user = apache/g" /etc/php-fpm.d/www.conf && sed -i -e "s/group\s*=.*/group = apache/g" /etc/php-fpm.d/www.conf

# In CentOS the http user is "apache"
RUN sed -i -e "s/user\s*=.*/user = apache/g" /etc/php-fpm.d/www.conf && sed -i -e "s/group\s*=.*/group = apache/g" /etc/php-fpm.d/www.conf

RUN sed -i "/^[ ]*PasswordAuthentication /c\PasswordAuthentication no" /etc/ssh/sshd_config

# Set root password because it is required from openssh,
# but password authentication using openssh is disabled,
# and the container access is permitted only for users
# of the group docker, on the host system.
RUN echo root:root | chpasswd

# Configure the access to the SSH server
ADD id_rsa.pub /root/id_rsa.pub
RUN ssh-keygen -b 1024 -t rsa -f /etc/ssh/ssh_host_key && ssh-keygen -b 1024 -t rsa -f /etc/ssh/ssh_host_rsa_key && ssh-keygen -b 1024 -t dsa -f /etc/ssh/ssh_host_dsa_key ; sed -ri 's/UsePAM yes/#UsePAM yes/g' /etc/ssh/sshd_config ; mkdir -p /root/.ssh ; cat /root/id_rsa.pub >> /root/.ssh/authorized_keys && chmod 700 /root/.ssh && chmod 640 /root/.ssh/authorized_keys

# Set MariaDB password
ADD set_mysql_password.sh /tmp/set_mysql_password.sh
RUN /bin/bash /tmp/set_mysql_password.sh

#
# These actions **must** be the last!!!
#

# Use the native file system, and a VOLUME for this directory.
VOLUME /var

EXPOSE 80 443 22

# Command to run supervisord in foreground
CMD ["/usr/bin/supervisord"]
