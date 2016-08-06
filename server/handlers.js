var readdirp = require('readdirp')
  , sys = require('util')
  , fs = require('fs')
  , path = require('path')
  , readline = require('readline')
  , url = require('url')
  , util = exports;

util.getMap = [];

util.get = function(path, handler) {
	util.getMap[path] = handler;
};

util.not_found = function(req, res) {
	var not_found_msg = 'Not Found';

	res.writeHead(404, {
		'Content-Type': 'text/plain',
		'Content-Length': not_found_msg.length
	});
	console.error(not_found_msg);
	res.write(not_found_msg);
	res.end();
};

util.staticHandler = function(filename) {
	var body;

	function loadResponseData(callback) {
		fs.readFile(filename, function(err, data) {
			if (err) {
				console.error('Error loadinf file ' + filename);
			} else {
				console.error('loading file ' + filename);
				body = data;
			}
			callback();
		});
	}

	return function(req, res) {
		loadResponseData(function() {
			res.writeHead(200, {
				'Content-Type': 'text/html',
				'Content-Length': body.length
			});
			res.end(body);
		});
	};

};

util.get('/', util.staticHandler('public/index.html'));
util.get('/favicon.ico', util.staticHandler('public/favicon.ico'));
util.get('favicon.ico', util.staticHandler('public/favicon.ico'));

util.get('/deps', function(req, res){
    var files = [];
    var id = 0;
    //res.writeHead(200, {'Content-Type': 'application/json'});
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
       res.simpleJSON(200, files);
    });

});
//util.get('/style.css', util.staticHandler('style.css'));
