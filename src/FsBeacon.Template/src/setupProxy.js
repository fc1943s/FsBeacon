const httpProxyMiddleware = require("http-proxy-middleware");

module.exports = function (app) {
  app.use(
    httpProxyMiddleware.createProxyMiddleware("/Sync", {
      target: "https://localhost:9761/",
      changeOrigin: true,
      secure: false,
      ws: true,
    })
  );
};
