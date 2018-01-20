'use strict';

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('root');

// .embed() can take an optional second argument
var app = Elm.Main.embed(mountNode);

