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


### API 

## Mixes list

`curl -i -H "Content-type: application/json" -X GET  localhost:8080/api/mixes`

response 

`[0,1,2,3,4,5]`

## Set all channels in mix to OFF

`curl -i -H "Content-type: application/json" -X POST  localhost:8080/api/mix/$MIXID$`

## Get info about mix

`curl -i -H "Content-type: application/json" -X GET  localhost:8080/api/mix/$MIXID$`

Response JSON: 

`[{"on":false,"image":"https://api.adorable.io/avatars/285/channel-1.png","id":0},
  {"on":false,"image":"https://api.adorable.io/avatars/285/channel-1.png","id":1},
  {"on":false,"image":"https://api.adorable.io/avatars/285/channel-1.png","id":2}]`

## Switch channel Of/Off

`curl -i -H "Content-type: application/json" -X POST  localhost:8080/api/channel/switch/0/3/on`

## Upload file

`curl -i  -F 'file=@erlang.mk' localhost:8080/api/channel/upload/1`