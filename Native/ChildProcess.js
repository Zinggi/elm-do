const _user$project$Native_ChildProcess = (() => {

const { exec } = require('child_process');

const executeCommand = (command) => {
    return _elm_lang$core$Native_Scheduler.nativeBinding((callback) => {
        exec(command, (err, stdout, stderr) => {
            if (err) {
                const msg = err.toString();
                callback(_elm_lang$core$Native_Scheduler.fail(msg));
                return;
            }
            callback(_elm_lang$core$Native_Scheduler.succeed(
                { stdOut: stdout, stdErr: stderr }
            ));
        });
    });
}

return {
	executeCommand: executeCommand
};

})();
