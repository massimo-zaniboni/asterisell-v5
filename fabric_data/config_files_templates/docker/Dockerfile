FROM centos:centos6
MAINTAINER Massimo Zaniboni - massimo.zaniboni@asterisell.com

# Broken sym-link (/var/lock) workaround
RUN mkdir -p /run/lock

# Install standard packages
RUN yum update -y ; yum -y groupinstall Base ; yum -y install git awk sqlite sqlite-devel rsync openssh-server

# IUS is a repository with last packages for Apache and PHP, mantained from RackSpace engineerings.
RUN yum install -y epel-release && wget https://centos6.iuscommunity.org/ius-release.rpm && rpm -Uvh ius-release*.rpm

# MariaDB is a community mantained fork of MySQL
ADD MariaDB.repo /etc/yum.repos.d/MariaDB.repo
RUN yum -y install MariaDB-server MariaDB-client MariaDB-devel

# Use NGINX version directly packaged from NGINX web site, because it is a lot more recent than CentOS version.
RUN wget http://nginx.org/keys/nginx_signing.key
RUN rpm --import nginx_signing.key
ADD nginx.repo /etc/yum.repos.d/nginx.repo
RUN yum --nogpgcheck -y install nginx

# Install IUS specific packages, and other related packages
RUN yum install -y httpd-tools php56u php56u-mysqlnd php56u-pdo php56u-cli php56u-common php56u-opcache php56u-bcmath php56u-xml php56u-mbstring php56u-gd php56u-fpm mingw32-iconv gmp lftp

# Install supervisor package (a recent version using python packages)
RUN yum install -y python-setuptools && easy_install supervisor
ADD supervisord.conf /etc/supervisord.conf

# Configure MariaDB
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

#
# Install Compilation Env
#

RUN yum groupinstall "Development Tools" -y ; yum install -y wget curl cmake libbsd-devel gmp-devel gmp zlib-devel make perl gcc which cmake Judy Judy-devel openssh-server glib2 glib2-devel clips clips-devel clips-libs openssl-devel git

# CAN-BE-UPGRADED: in case update the version of .so version
RUN cd /usr/lib64/ && ln -s libmysqlclient.so.16.0.0 libmysqlclient.so

# Install RAGEL
ADD ragel-6.8.tar.gz /root/
RUN cd /root/ragel-6.8 && ./configure && make && make install

# Install Haskell Stack Environment
RUN curl -sSl https://s3.amazonaws.com/download.fpcomplete.com/centos/6/fpco.repo | tee /etc/yum.repos.d/fpco.repo && yum -y install stack && cd && stack setup && stack install cabal-install

ENV PATH /local/bin:/root/.local/bin:/root/.cabal/bin:$$PATH

# Precompile Haskell application so all Stack packages are downloaded
# and the big compilation environment is shared between multiple
# containers.
# NOTE: rate_engine will be copied from the fabfile tool.
ADD rate_engine_packages.tar.gz /root/
RUN chown -R root:root /root/rate_engine_packages
RUN cd /root/rate_engine_packages && stack setup
RUN cd /root/rate_engine_packages && stack build || :

#
# Actions that are likely to require a rebuild of the image.
#

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
# Final Configuration Actions
#

# Use the native file system, and a VOLUME for this directory.
VOLUME /var

EXPOSE 80 443 22

# Command to run supervisord in foreground
CMD ["/usr/bin/supervisord"]
