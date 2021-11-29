yarn install

# yarn mkcert -install

[System.IO.Directory]::CreateDirectory('ssl')
# yarn mkcert --cert-file=./ssl/$env:FSBEACON_DOMAIN.pem --key-file=./ssl/$env:FSBEACON_DOMAIN-key.pem $env:FSBEACON_DOMAIN
yarn mkcert create-ca --cert=./ssl/ca.pem --key=./ssl/ca-key.pem
yarn mkcert create-cert --ca-cert=./ssl/ca.pem --ca-key=./ssl/ca-key.pem --cert=./ssl/$env:FSBEACON_DOMAIN.pem --key=./ssl/$env:FSBEACON_DOMAIN-key.pem --domains $env:FSBEACON_DOMAIN

node server.js
