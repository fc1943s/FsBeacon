yarn install

yarn mkcert -install

[System.IO.Directory]::CreateDirectory('ssl')
yarn mkcert --cert-file=./ssl/$env:FSBEACON_DOMAIN.pem --key-file=./ssl/$env:FSBEACON_DOMAIN-key.pem $env:FSBEACON_DOMAIN

node server.js
