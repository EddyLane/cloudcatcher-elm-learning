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

const initiateElmApp = getStorage => {

	const app = Elm.fullscreen(Elm.Main, { getStorage });

    app.ports.fullModelChanges.subscribe((model) => {
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
        Promise.all(images.map(loadImage)).then((images) => {
					images.forEach((image) => imageDb.lru.put(image.uri, image.blob, { type: 'image/jpeg' }););
        );
    });

};

const retrieveImage = uri => new Promise((resolve, reject) => {
	const fileReader = new FileReader();
	fileReader.onload = (evt) => resolve(evt.target.result);
	imageDb.lru.get(uri).then((blob) => fileReader.readAsDataUrl(blob));
});

const loadImage = uri => new Promise((resolve, reject) => {
    const xhr = new XMLHttpRequest();
    xhr.responseType = 'blob';
    xhr.onload = () => resolve({
			uri,
			blob: new Blob([xhr.response], { type: "image/jpeg" })
		});
    xhr.open('GET', uri, true);
    xhr.send();
});

db.get(STATE_KEY)
	.then(doc => initiateElmApp(doc))
	.catch(err => initiateElmApp(null));
