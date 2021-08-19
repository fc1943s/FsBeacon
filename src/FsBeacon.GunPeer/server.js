const cluster = require('cluster');
const fs = require('fs');
const path = require('path');

if (cluster.isMaster) {
  return cluster.fork() && cluster.on('exit', () => {
    cluster.fork();
    require('gun/lib/crashed');
  });
}

console.log('argv', process.argv);

if (process.argv[2] !== "--root-path") {
  throw new Error('Invalid --root-path');
}

const rootPath = process.argv[3] || process.env.ROOT_PATH;
if (!rootPath || rootPath === "%ROOT_PATH%") {
  throw new Error('Invalid --root-path');
}

if (process.argv[4] !== "--domain") {
  throw new Error('Invalid --domain');
}

const domain = process.argv[5] || process.env.FSBEACON_DOMAIN;
if (!domain || rootPath === "%FSBEACON_DOMAIN%") {
  throw new Error('Invalid --domain');
}

if (!fs.existsSync(rootPath)) {
  fs.mkdirSync(rootPath);
}

const config = {
  port: process.env.PORT || "8765"
};
const Gun = require('gun');

if (process.env.HTTPS && domain) {
  config.cert = fs.readFileSync('./ssl/' + domain + '.pem');
  config.key = fs.readFileSync('./ssl/' + domain + '-key.pem');
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
  ' domain=' + domain +
  ' rootPath=' + rootPath +
  ' path.resolve(rootPath)=' + path.resolve(rootPath) +
  ' https: ' + Boolean(process.env.HTTPS));

module.exports = gun;
