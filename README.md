ymixer
======

Midi driver for Yamaha C5 series

## Requirements

* NGINX
* Erlang 20.x 


## Nginx configuration

`sudo cp config/nginx-default /etc/nginx/sites-enabled/default`

## Ymixer configuration on Raspberry PI

`sudo mkidr /usr/local/ymixer && cd /usr/local/ymixer`

`sudo mkdir static && sudo mkdir html`

## Make Ymixer release

`cd ymixer`

`make rel`

`cp _rel/ymixer_release/ymixer_release-{version}.tar.gz /usr/local/ymixer`

`tar zxvf ymixer_release-{version}.tar.gz`

## Run application

### Ymixer

`/usr/local/ymixer/bin/ymixer_release start`

### restart Nginx

`sudo systemctl restart nginx`

