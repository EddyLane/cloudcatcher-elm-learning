/// <reference path="../../" />

import * from "typescript";
import PouchDB from 'pouchdb';
import * as pouchdbLruCache from 'pouchdb-lru-cache';

PouchDB.plugin(pouchdbLruCache);

const db = new PouchDB('cloudcatcher');
const imageDb = new PouchDB('images');

imageDb.initLru(5000000); // store 5 MB maximum
imageDb.initLru(0); // no limit


const STATE_KEY = 'state';

imageDb.lru.info().then(function (info) {
	console.debug(info);
});

const initiateElmApp = getStorage => {

	const app = Elm.fullscreen(Elm.Main, { getStorage, addImageTwo: { uri: "Banter", data: "Boobs" } });

	app.ports.fullModelChanges.subscribe((model) => {

		console.log(model);
		db.get(STATE_KEY)
		.then(doc => db.put(Object.assign({}, model, {
			_id: STATE_KEY,
			_rev: doc._rev
		})))
		.catch(e => db.put(Object.assign({}, model, {
			_id: STATE_KEY
		})))
		;
	});


	app.ports.incomingImages.subscribe((images) => {
		console.debug('Incoming images', images);
			getAllImages(images).then(imageData => imageData.map(imageData => {
					return app.ports.addImageTwo.send(imageData);
			}))
	});

};



const getAllImages = images => {

	return Promise.all(images.map(uri => {
		return imageDb.lru.has(uri).then(exists => {
			if (exists) {
				return retrieveImage(uri);
			} else {
				return loadImage(uri);
			}
		});
	}))

}


const retrieveImage = uri => new Promise((resolve, reject) => {

console.log('retrieving', uri);

	const fileReader = new FileReader();
	fileReader.onload = evt => {
		resolve({
			uri,
			data: evt.target.result
		});
	};
	imageDb.lru.get(uri).then((blob) => fileReader.readAsDataURL(blob));
});

const loadImage = uri => new Promise((resolve, reject) => {
	console.log('loading', uri);

	const xhr = new XMLHttpRequest();
	xhr.responseType = 'blob';

	xhr.onload = () => {
		imageDb.lru.put(uri, blob, { type: 'image/jpeg' });
		resolve(retrieveImage(uri));
	};

	xhr.open('GET', uri, true);
	xhr.send();

});

db.get(STATE_KEY)
.then(doc => initiateElmApp(doc))
.catch(err => initiateElmApp(null));
