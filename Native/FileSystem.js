const _user$project$Native_FileSystem = (() => {

const fs = require('fs');

const readDirectory = (dir) => {
    return _elm_lang$core$Native_Scheduler.nativeBinding((callback) => {
        fs.readdir(dir, (error, data) => {
            if (error) {
                const msg = error.toString();
                callback(_elm_lang$core$Native_Scheduler.fail(msg));
                return;
            }
            callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_List.fromArray(data)));
        });
    });
}


const fileType = ["IsFile", "IsDirectory", "Other"];
const description = (path) => {
    return _elm_lang$core$Native_Scheduler.nativeBinding((callback) => {
        fs.stat(path, (error, stats) => {
            if (error) {
                const msg = error.toString();
                callback(_elm_lang$core$Native_Scheduler.fail(msg));
                return;
            }
            let type = 2;
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

const readFile = (options, path) => {
    return _elm_lang$core$Native_Scheduler.nativeBinding((callback) => {
        let encoding = null;
        if (options.encoding.ctor == 'Just') {
            encoding = options.encoding._0;
        }
        fs.readFile(path, encoding, (error, data) => {
            if (error) {
                const msg = error.toString();
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

})();
