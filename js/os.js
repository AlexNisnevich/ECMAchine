/*
 * The OS singleton performs power- and state-related operations
 */
var OS = {
  initialize: function() {
    Display.echo("Launching ECMAchine ...");
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
      Display.echo("Restoring filesystem state ...");
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
 * The display singleton interacts with the terminal
 */
var Display = {
  terminal: null, // terminal object
  lineContinuationString: '.. ',
  
  initialize: function(term) {
    this.terminal = term;
  },
  
  /*
   * Preprocesses display output
   */
  preprocess: function(str) {
  	if (!str) {
  		return null;
  	} else if (str.isString) {
  		str = str.toDisplayString();
  	} else {
  		str = str.toString();
  	}
  	str = str.replace(/lambda/g, '&lambda;').replace(/true/g, '#t').replace(/false/g, '#f');
  	return str;
  },
  
  /*
   * Process and display output
   */
  echo: function(str) {
    str = this.preprocess(str);
    if (str != null) {
    	this.terminal.echo(str);
    	this.refresh();
    }
  },
  
  /*
   * Refresh terminal: resize + scroll to bottom
   */
  refresh: function() {
    // timeout needed to avoid some silly UI issues
    setTimeout( function() { 
      Display.terminal.resize();
      $(document).scrollTop($(document).height());
    }, 100);
  },
  
  /*
   * Resize terminal to avoid overlap with overlays
   */
  resize: function() {
    if ($('.overlayRight').length == 0) {
      var newWidth = $(document).width() - 20;
    } else {
      var newWidth = $('.overlayRight').map(function (i, x) {return x.offsetLeft;}).sort(function (a,b) {return a - b})[0] - 20;
    }
    
    if (this.terminal.width() != newWidth) {
      this.terminal.resize(newWidth, this.terminal.height());
    }
  },
  
  /*
   * Adds a new line and removes previous command from output and history
   */
  newline: function(command) {
    this.terminal.set_command(command + '\n' + this.lineContinuationString); // start new line in command
    this.terminal.lines(this.terminal.lines().slice(0,-1)); // remove last line in output
    for (i = 1; i < command.split(/\n/).length; i++) {
      this.terminal.history().pop(); // need to pop once for each line after the first
    }
    this.terminal.resize(); // refresh terminal
  },
  
  debugPrint: function(txt) {
  	if (Processes.getCurrentProcess().isTerminal) {
  		console.log(txt);
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
    Display.echo('Launching filesystem ...');
    
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

/*
 * The Processes singleton handles processes
 */
var Processes = {
	processList: [],
	currentPID: null,
	
	terminalProcess: {
		'pid': -1,
		'name': 'Terminal',
		'isTerminal': true,
		
		// Performance
		'timeStarted': new Date().getTime(),
		'timeElapsed': function () { return ((new Date().getTime()) - this.timeStarted); },
		'evals': 0
	},
	
	getProcessByID: function(pid) {
		if (pid == -1 || pid == null) {
			return this.terminalProcess;
		} else if (this.processList[pid] === undefined || this.processList[pid].terminated) {
			throw 'There is no process with PID ' + pid;
		} else {
			return this.processList[pid];
		}
	},
	
	getCurrentProcess: function() {
		return this.getProcessByID(this.currentPID);
	},
	
	setCurrentPID: function(pid) {
		if (pid !== undefined) {
			Processes.currentPID = pid;
		} else {
			Processes.currentPID = null;
		}
	},
	
	/*
	 * Increments the evals count of the current process
	 */
	incrementEvals: function() {
		var process = this.getCurrentProcess();
		process.evals++;
	},
	
	/*
	 * Starts a new process, returns its PID
	 */
	startProcess: function(name, contents, refreshRate) {
		var pid = this.processList.length;
		
		// start interval
		var interval = setInterval(function () {
			var result = evaluate(contents, pid);
			if (result !== undefined) {
				Display.echo(result);
			}
		}, refreshRate);
		
		// add to process list
		this.processList.push({
			'pid': pid,
			'name': Filesystem.getNameFromPath(name),
			'process': interval,
			'code': contents,
			'terminated': false,
			'overlays': [],
			
			// Performance
			'timeStarted': new Date().getTime(),
			'timeElapsed': function () { return ((new Date().getTime()) - this.timeStarted); },
			'interval': refreshRate,
			'evals': 0
		});
		
		return pid;
	},
	
	/*
	 * Kills a process by PID, returns result
	 */
	killProcess: function(pid) {
		var proc = this.getProcessByID(pid);
		if (proc.isTerminal) {
			throw 'Can\'t kill terminal';
		}
		clearInterval(proc.process);
		proc.terminated = true;
		
		// remove associated overlays
		proc.overlays.forEach(function (name) {
			$('#overlays #' + name).remove();}
		);
		
		return new Array('Process with PID ' + pid + ' [' + proc.name + '] terminated');
	},
	
	/*
	 * Returns a list of running processes
	 */
	listProcesses: function() {
		var procs = this.processList.filter(function (proc) {return !proc.terminated});
		procs.unshift(this.terminalProcess);
		return procs;
	},
	
	/*
	 * Returns the evals/sec performance of a process by PID
	 */
	getPerformance: function(pid) {
		var proc = this.getProcessByID(pid);
		var evalsPerMS = proc.evals / (proc.timeElapsed());
		var evalsPerSec = Math.round(evalsPerMS * 1000000)/1000;
		return evalsPerSec;
	},
	
	/*
	 * Registers a named overlay as belonging to the current process
	 */
	registerOverlay: function(name) {
		if (this.currentPID != null && this.getProcessByID(this.currentPID).overlays.indexOf(name) < 0) {
			this.getProcessByID(this.currentPID).overlays.push(name);
		}
	}
}
