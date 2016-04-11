var mime = require('mime');

module.exports = {
  files: ['dist/*.(html|css|js)'],
  //https: true,
  port: 1337,
  server: {
    baseDir: 'dist',
    middleware: [
      require('connect-history-api-fallback')(),
      function (req, res, next) {
        var path = req._parsedUrl.pathname;
        var type = mime.lookup(path);
        var charset = mime.charsets.lookup(type);
        res.setHeader('Content-Type', type + (charset ? '; charset=' + charset : ''));
        next()
      }
    ]
  }
}
