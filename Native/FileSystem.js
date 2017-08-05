var _user$project$Native_FileSystem = function() {

const fs = require('fs');

var readDirectory = function(dir) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
        fs.readdir(dir, function(error, data) {
            if (error) {
                var msg = error.toString();
                callback(_elm_lang$core$Native_Scheduler.fail(msg));
                return;
            }
            callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_List.fromArray(data)));
        });
    });
}


var fileType = ["IsFile", "IsDirectory", "Other"];
var description = function(path) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
        fs.stat(path, function(error, stats) {
            if (error) {
                var msg = error.toString();
                callback(_elm_lang$core$Native_Scheduler.fail(msg));
                return;
            }
            var type = 2;
            if (stats.isFile()) {
                type = 0;
            } else if (stats.isDirectory()) {
                type = 1;
            }
            callback(_elm_lang$core$Native_Scheduler.succeed(
                { type_: { ctor: fileType[type] } }
            ));
        });
    });
}

var readFile = function(options, path) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
        var encoding = null;
        if (options.encoding.ctor == 'Just') {
            encoding = options.encoding._0;
        }
        fs.readFile(path, encoding, function(error, data) {
            if (error) {
                var msg = error.toString();
                callback(_elm_lang$core$Native_Scheduler.fail(msg));
                return;
            }

            callback(_elm_lang$core$Native_Scheduler.succeed(data));
        });
    });
}

return {
	readDirectory: readDirectory,
    description: description,
    readFile: F2(readFile)
};

}();
