FROM nginx:mainline-alpine

ARG WWW_HERIGONE_COM_PEM
ARG WWW_HERIGONE_COM_KEY

COPY static /var/www/www.herigone.com
COPY conf/etc/nginx/nginx.conf /etc/nginx/nginx.conf
COPY $WWW_HERIGONE_COM_PEM /etc/nginx/keys/www_herigone_com.pem
COPY $WWW_HERIGONE_COM_KEY /etc/nginx/keys/www_herigone_com.key
