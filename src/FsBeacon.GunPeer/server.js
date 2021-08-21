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

const config = (() => {
  const port = process.env.PORT || "8765";
  const rootPath = process.argv[3] || process.env.ROOT_PATH;
  const domain = process.argv[5] || process.env.FSBEACON_DOMAIN;
  const https = Boolean(process.env.HTTPS && domain);
  return { port, rootPath, domain, https }
})();

console.log("config", JSON.stringify(config, null, 4));

if (!config.rootPath || config.rootPath === "%ROOT_PATH%") {
  throw new Error('Invalid --root-path');
}
if (!config.domain || config.domain === "%FSBEACON_DOMAIN%") {
  throw new Error('Invalid --domain');
}
if (!fs.existsSync(config.rootPath)) {
  fs.mkdirSync(config.rootPath);
}

const Gun = require('gun');

const server = config.https
  ? require('https').createServer(
    {
      cert: fs.readFileSync(`./ssl/${config.domain}.pem`),
      key: fs.readFileSync(`./ssl/${config.domain}-key.pem`),
    },
    Gun.serve(__dirname)
  )
  : require('http').createServer(Gun.serve(__dirname))

// config.server.configure(function () {
//   config.server.use(function(req, res, next) {
//     res.header("Access-Control-Allow-Origin", "https://localhost/49212");
//     res.header("Access-Control-Allow-Origin", "https://localhost/49222");
//     res.header("Access-Control-Allow-Headers", "X-Requested-With");
//     next();
//   });
// });


const gun = Gun({
  web: server.listen(config.port),
  // stats: false,
  file: config.rootPath
});

console.log(`Relay peer started. URL=/gun
path.resolve(rootPath)=${path.resolve(config.rootPath)} `);

module.exports = gun;
