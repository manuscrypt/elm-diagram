var http = require('http'),
    url = require('url'),
    port = process.env.PORT || 3001,
    handlers = require('./handlers');

//  , https = require('https')
// var privateKey  = fs.readFileSync('/etc/ssl/private/server.key', 'utf8');
// var certificate = fs.readFileSync('/etc/ssl/certs/server.crt', 'utf8');
// var credentials = {key: privateKey, cert: certificate};

http.createServer(function(req, res) {
    var pathname = url.parse(req.url).pathname
    console.log(pathname);
    var handler = handlers.getMap[pathname] || handlers.not_found;
    console.log(handler);
    res.simpleJSON = function(code, obj) {
        var body = JSON.stringify(obj);
        res.writeHead(code, {
            'Content-Type': 'application/json',
            'Content-Length': body.length
        });
        res.write(body);
        res.end();
    };

    handler(req, res);
    // res.setHeader('Access-Control-Allow-Origin', '*');
    // res.setHeader('Access-Control-Request-Method', '*');
    // res.setHeader('Access-Control-Allow-Methods', 'OPTIONS, GET');
    // res.setHeader('Access-Control-Allow-Headers', '*');

}).listen(port, function() {
    console.log('Http listening on ' + port);
});

// var httpsServer = https.createServer(credentials,app);
// httpsServer.listen(port, function(){
//    console.log('Https listening on port ' + port);
// });
