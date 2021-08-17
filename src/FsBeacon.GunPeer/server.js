const cluster = require('cluster');
if (cluster.isMaster) {
  return cluster.fork() && cluster.on('exit', function () {
    cluster.fork();
    // require('gun/lib/crashed');
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
if(!fs.existsSync(rootPath)) {
  fs.mkdirSync(rootPath);
}

const config = {
  port: process.env.PORT || "8765"
};
const Gun = require('gun');

if (process.env.HTTPS && process.env.FSBEACON_DOMAIN) {
  config.cert = fs.readFileSync('./ssl/' + process.env.FSBEACON_DOMAIN + '.pem');
  config.key = fs.readFileSync('./ssl/' + process.env.FSBEACON_DOMAIN + '-key.pem');
  config.server = require('https').createServer(config, Gun.serve(__dirname));
  // config.server.configure(function () {
  //   config.server.use(function(req, res, next) {
  //     res.header("Access-Control-Allow-Origin", "https://localhost/49212");
  //     res.header("Access-Control-Allow-Origin", "https://localhost/49222");
  //     res.header("Access-Control-Allow-Headers", "X-Requested-With");
  //     next();
  //   });
  // });
} else {
  config.server = require('http').createServer(Gun.serve(__dirname));
}


const gun = Gun({
  web: config.server.listen(config.port),
  // stats: false,
  file: rootPath
});
console.log('Relay peer started on port ' + config.port + ' with /gun. ' +
  ' FSBEACON_DOMAIN=' + process.env.FSBEACON_DOMAIN +
  ' rootPath=' + rootPath +
  ' https: ' + Boolean(process.env.HTTPS));

module.exports = gun;
