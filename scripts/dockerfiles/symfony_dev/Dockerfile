FROM centos:centos7
MAINTAINER Massimo Zaniboni - massimo.zaniboni@asterisell.com

# From https://bugzilla.redhat.com/show_bug.cgi?id=1213602
RUN touch /var/lib/rpm/*

RUN yum update -y ; yum -y groupinstall Base ; yum -y install git awk sqlite sqlite-devel rsync

RUN yum install -y httpd-tools php php-mysqlnd php-pdo php-cli php-common php-opcache php-bcmath php-xml php-mbstring php-gd php-fpm mingw32-iconv gmp lftp

RUN yum groupinstall "Development Tools" -y ; yum install -y wget curl libbsd-devel gmp-devel gmp zlib-devel make perl gcc which openssh-server glib2 glib2-devel openssl-devel git

CMD ["/bin/bash"]
