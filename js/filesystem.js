var Filesystem = {
	fs: globalEnvironment['__fileSystem'], // path to file system
	currentDir: globalEnvironment['__currentDir'], // current directory
	
	//
	// HELPER FUNCTIONS
	//
		
	/*
	 * Gets new path (e.g. for 'cd' command)
	 */
	calculatePath: function(dir) {
		if (dir == '') {
			return this.currentDir;
		} if (dir == '/') {
			return '/';
		} else {
			if (dir == "'") { dir = dir.slice(1); } // remove initial quote if exists 
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
		return this.calculatePath(pathSplit.slice(0, pathSplit.length - 1).join('/'));
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
		
		if (file.type == 'dir') {
			this.setDir(this.getDir());
		} else {
			var contents = file.contents;
			this.setFile(newPath, { 'type': 'file', 'contents': contents });
		}
		
		return {
			'oldPath': oldPath,
			'newPath': newPath
		};
	}
}
