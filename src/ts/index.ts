/// <reference path="../../typings/tsd.d.ts" />

import * from "typescript";
import PouchDB from 'pouchdb';

const db = new PouchDB('cloudcatcher');

const STATE_KEY = 'state';

const initiateElmApp = (getStorage) => {

	const app = Elm.fullscreen(Elm.Main, {
        getStorage
    });

    app.ports.fullModelChanges.subscribe(function (model) {

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

};


db.get(STATE_KEY)
.then(doc => initiateElmApp(doc))
.catch(err => initiateElmApp(null))
;



//app.ports.modelChanges.subscribe(function (model) {
//    console.log(model);
//});



