const cluster = require('cluster');
if (cluster.isMaster) {
  return cluster.fork() && cluster.on('exit', function () {
    cluster.fork();
    require('../lib/crashed');
  });
}

console.log('argv', process.argv);
if (process.argv[2] !== "--root-path") {
  throw new Error('Invalid --root-path');
}

const rootPath = process.argv[3];
if (!rootPath) {
  throw new Error('Invalid --root-path');
}

const fs = require('fs');
const config = {
  port: process.env.PORT || "8765"
};
const Gun = require('gun');

if (process.env.HTTPS) {
  config.cert = fs.readFileSync('./ssl/' + process.env.FSBEACON_DOMAIN + '.pem');
  config.key = fs.readFileSync('./ssl/' + process.env.FSBEACON_DOMAIN + '-key.pem');
  config.server = require('https').createServer(config, Gun.serve(__dirname));
} else {
  config.server = require('http').createServer(Gun.serve(__dirname));
}

const gun = Gun({
  web: config.server.listen(config.port),
  file: rootPath
});
console.log('Relay peer started on port ' + config.port + ' with /gun. ' +
  ' FSBEACON_DOMAIN=' + process.env.FSBEACON_DOMAIN +
  ' rootPath=' + rootPath +
  ' https: ' + Boolean(process.env.HTTPS));

module.exports = gun;
