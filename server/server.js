var readdirp = require('readdirp')
  , path = require('path')
  , es = require('event-stream')
  , http = require('http')
  , fs = require('fs')
  , readline = require('readline')
  , port = process.env.PORT || 3001
  , https = require('https')

var privateKey  = fs.readFileSync('/etc/ssl/private/server.key', 'utf8');
var certificate = fs.readFileSync('/etc/ssl/certs/server.crt', 'utf8');
var credentials = {key: privateKey, cert: certificate};

const app = function (req, res) {

    res.setHeader('Access-Control-Allow-Origin', '*');
	res.setHeader('Access-Control-Request-Method', '*');
	res.setHeader('Access-Control-Allow-Methods', 'OPTIONS, GET');
	res.setHeader('Access-Control-Allow-Headers', '*');

    var files = [];
    var id = 0;
    res.writeHead(200, {'Content-Type': 'application/json'});
    readdirp({ root: path.join(__dirname, '..'), fileFilter: '*.elm' }).on('data', function (entry) {
        var summ = { id: id++, moduleName: "", name: entry.name, path: entry.path, size: entry.stat.size, imports : [] };
        readline.createInterface({
            input: fs.createReadStream(entry.fullPath),
            terminal: false
        }).on('line', function(line) {
            // diese variablen ausserhalb erkennt nicht alle imports
            //const idRegex = /\b(?:\S+?\.)+\S+\b/g
            const moduleRegex = /^module\s+([\w*|\.]*)\b/g
            const importRegex = /^import\s+([\w*|\.]*)\b/g

            var moduleMatch = moduleRegex.exec(line);
            if( moduleMatch ){
                summ.moduleName = moduleMatch[1];
            }
            var importMatch = importRegex.exec(line);
            if( importMatch ){
                summ.imports.push( importMatch[1] );
            }
        }).on('close', function(){
            if( summ.moduleName !== "" ){
              files.push(summ);
            }
        });
    }).on( 'end', function() {
        res.end( JSON.stringify(files));
    });
};

var httpServer = http.createServer(app);
httpServer.listen(8080, function(){
  console.log('Http listening on 8080');
});

var httpsServer = https.createServer(credentials,app);
httpsServer.listen(port, function(){
   console.log('Https listening on port ' + port);
});
