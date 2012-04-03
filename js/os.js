/*
 * The OS singleton performs power- and state-related operations
 */
var OS = {
	initialize: function() {
		terminalEcho("Launching ECMAchine ...");
		initEvaluator();
	  this.loadState();
		Filesystem.initialize();
		
		// start automatically saving filesystem to localStorage every 60 sec
		setInterval(OS.saveState, 60000);
	},
	
	close: function() {
		window.open('', '_self', ''); // Arthur Filliot's suggestion - allows window.close() to work in Chrome
		window.close(); 
	},
	
	reboot: function() {
		location.reload(true);
	},
	
	loadState: function() {
		if (typeof localStorage != "undefined" && typeof localStorage.fileSystemFrame != "undefined") {
			terminalEcho("Restoring filesystem state ...");
			var fileSystemFrame = JSON.parse(localStorage.fileSystemFrame);
	 } else {
	 		var fileSystemFrame = defaultFileSystemFrame;
	 }
	 Filesystem.importFSFrame(fileSystemFrame);
	},
	
	saveState: function() {
		if (typeof localStorage != "undefined") {
			localStorage.fileSystemFrame = JSON.stringify(Filesystem.exportFSFrame());
	  }
	},
	
	deleteSavedState: function() {
		if (typeof localStorage != "undefined") {
			delete localStorage.fileSystemFrame;
	  }
	}
}

/*
 * The Filesystem singleton performs filesystem-related operations
 */
var Filesystem = {
	fs: null, // path to file system
	currentDir: '/', // current directory
	
	/*
	 * Initializes filesystem (called from OS.initialize() )
	 */
	initialize: function() {
		terminalEcho('Launching filesystem ...');
		
		// Run all files in /startup
		for (var fname in Filesystem.getDir('/startup')) {
			var contents = Filesystem.getFile('/startup/' + fname).contents;
			evaluate(contents);
		}
	},
	
	//
	// HELPER FUNCTIONS
	//
	
	importFSFrame: function(fileSystemFrame) {
		this.fs = fileSystemFrame['__fileSystem'];
		
		// Add file system frame to environment
		globalEnvironment.push(fileSystemFrame);
	},
	
	exportFSFrame: function() {
		return {'__fileSystem': this.fs};
	},
		
	/*
	 * Gets new path (e.g. for 'cd' command)
	 */
	calculatePath: function(dir) {
		if (dir == '') { // current dir
			return this.currentDir;
		} else if (dir == '/') { // top-level dir
			return '/';
		} else if (dir[0] == '/') { // calculate from top-level dir
			return dir;
		} else { // calculate from current dir
			var pathComponents = this.currentDir.split('/');
			var dirComponents = dir.split('/');
			dirComponents.forEach(function (comp) {
				if (comp == '..') {
					pathComponents.pop();
				} else {
					pathComponents.push(comp);
				}
			});
			var newPath = pathComponents.join('/').replace(/\/+/g,'/');
			return (newPath != '') ? newPath : '/';
		}
	},
	
	/*
	 * e.g. '/dir1/dir2/file' => 'file'
	 */
	getNameFromPath: function(path) {
		var pathSplit = path.split('/');
		return pathSplit[pathSplit.length - 1];
	},
	
	/*
	 * e.g. '/dir1/dir2/file' => '/dir1/dir2'
	 */
	getFolderFromPath: function(path) {
		var pathSplit = path.split('/');
		if (path[0] == '/') { // top-level dir
			var folder = pathSplit.slice(0, pathSplit.length - 1).join('/');
			return (folder == '')? '/' : folder;
		} else {
			return this.calculatePath(pathSplit.slice(0, pathSplit.length - 1).join('/'));
		}
	},
	
	/*
	 * Gets a file from a path
	 */
	getFile: function(path) {
		return this.fs[this.getFolderFromPath(path)][this.getNameFromPath(path)];
	},
	
	/*
	 * Gets all files in directory
	 */
	getDir: function(dir) {
		return this.fs[dir];
	},
	
	/*
	 * Creates/updates file at given path
	 */
	setFile: function(path, file) {
		this.fs[this.getFolderFromPath(path)][this.getNameFromPath(path)] = file;
	},
	
	/*
	 * Creates/updates dir at given path
	 */
	setDir: function(path, dir) {
		this.fs[path] = dir;
	},
	
	/*
	 * Deletes file at path
	 */
	deleteFileAtPath: function(path) {
		delete this.fs[this.getFolderFromPath(path)][this.getNameFromPath(path)];
	},
	
	/*
	 * Deletes directory
	 */
	deleteDir: function(path) {
		delete this.fs[path];
	},
	
	//
	// EXCEPTIONS
	//
	
	
	/*
	 * Throws exception if path not valid
	 */
	checkPathExists: function(path) {
		if (this.fs[path] === undefined) {
			throw 'File system error: path "' + path + '" does not exist';
		}
	},
	
	/*
	 * Throws exception if file doesn't exist
	 */
	checkFileExists: function(file, path) {
		if (file === undefined) {
			throw 'File system error: file "' + path + '" does not exist';
		}
	},
	
	/*
	 * Throws exception if file is a directory
	 */
	checkNotADir: function(file, path) {
		if (file !== undefined && file.type == 'dir') {
			throw 'File system error: "' + path + '" is a directory';
		}
	},
	
	/*
	 * Throws exception if file/dir already exists
	 */
	checkAlreadyExists: function(file, path) {
		if (file !== undefined) {
			throw 'Error: "' + path + '" already exists';
		}
	},
	
	//
	// FILESYSTEM FUNCTIONS
	//
	
	/*
	 * Lists files in directory
	 */
	listFiles: function(dir) {
		var workingDir = dir ? this.calculatePath(dir) : this.currentDir;
		this.checkPathExists(workingDir);
		
		var fileNames = [];
		for (var fname in this.getDir(workingDir)) {
			fileNames.push(fname);
		}
		fileNames.sort();
		return fileNames;
	},
	
	/*
	 * Changes the current directory
	 * Returns the new path
	 */
	navigate: function(path) {
		var newPath = this.calculatePath(path);
		this.checkPathExists(newPath);
		this.currentDir = newPath;
		return newPath;
	},
	
	/*
	 * Returns file contents
	 */
	readFile: function(path) {
		var file = this.getFile(path);
		this.checkFileExists(file, path);
		this.checkNotADir(file, path);
		return file.contents;
	},
	
	/*
	 * Creates a directory
	 * Returns its path
	 */
	makeDir: function(name) {
		var newDirPath = this.calculatePath(name);
		this.checkAlreadyExists(this.getDir(name), newDirPath);
		this.setDir(newDirPath, {});
		this.setFile(name, { 'type': 'dir' });
		return newDirPath;
	},
	
	/*
	 * Creates new file
	 * Returns its path
	 */
	newFile: function(path) {
		var file = this.getFile(path);
		this.checkAlreadyExists(file);
		this.setFile(path, { 'type': 'file', 'contents': '' });
		return this.calculatePath(path);
	},
	
	/*
	 * Saves the file
	 * Returns its path
	 */
	saveFile: function(path, contents) {
		var file = this.getFile(path);
		this.checkNotADir(file);
		this.setFile(path, { 'type': 'file', 'contents': contents });
		return this.calculatePath(path);
	},
	
	/*
	 * Removes a file or directory
	 * Returns its path
	 */
	removeItem: function(path) {
		var file = this.getFile(path);
		this.checkFileExists(file, path);
		if (file.type == 'dir') {
			var dirPath = this.calculatePath(path);
			this.deleteDir(dirPath);
		}
		this.deleteFileAtPath(path);
		return this.calculatePath(path);
	},
	
	/*
	 * Copies a file or directory to a new path
	 * Returns {oldPath, newPath}
	 */
	copyItem: function(path, newPath) {
		var file = this.getFile(path);
		var oldPath = this.calculatePath(path);
		var newPath = this.calculatePath(newPath);
		var newFolderPath = this.getFolderFromPath(newPath);
		
		this.checkFileExists(file, path);
		this.checkPathExists(newFolderPath);
		
		// if copying to a dir, append current filename to path
		if (this.getFile(newPath) && this.getFile(newPath).type == 'dir') {
			newPath = newPath + '/' + this.getNameFromPath(path);
		}
		
		if (file.type == 'dir') {
			this.setDir(newPath, this.getDir(oldPath));
			this.setFile(newPath, { 'type': 'dir' });
		} else {
			this.setFile(newPath, { 'type': 'file', 'contents': file.contents });
		}
		
		return {
			'oldPath': oldPath,
			'newPath': newPath
		};
	}
};
