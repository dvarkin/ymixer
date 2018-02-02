'use strict';

import './index.html'
import './material.css'
import {uploadImage} from './uploader'

var Elm = require('./Main.elm');
var mountNode = document.getElementById('root');

// .embed() can take an optional second argument
var app = Elm.Main.embed(mountNode);
// var app = Elm.Main.fullscreen();
// var app = Elm.Main.worker();

app.ports.uploadImage.subscribe(
	function(channelId) {
		uploadImage(channelId);
	}
);