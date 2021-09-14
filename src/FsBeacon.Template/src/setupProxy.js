// TODO: remove file

const httpProxyMiddleware = require("http-proxy-middleware");

module.exports = function (app) {
  app.use(
    httpProxyMiddleware.createProxyMiddleware("/fsbeacon", {
      target: "https://localhost:49221/",
      changeOrigin: true,
      secure: false,
      ws: true,
    })
  );
};
