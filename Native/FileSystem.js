var _user$project$Native_FileSystem = function() {

const fs = require('fs');

var listFiles = function(dir) {
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

return {
	listFiles: listFiles,
};

}();
