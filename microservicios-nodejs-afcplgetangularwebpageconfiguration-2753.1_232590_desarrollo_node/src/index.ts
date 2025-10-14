import "reflect-metadata";
import express from "express";
import bodyParser from "body-parser";
import { Request, Response } from "express";
import { Routes } from "./routes";
import structure from './source/data';
import fs from 'fs';
import { createServer } from 'https';

const cors = require('cors');
let msgLog: string = `::${structure.APP_CONFIGANGULARWEPAGE_APP_NAME}::`;
try {
    const privateKey = fs.readFileSync(structure.APP_MICROSERVICES_KEY);
    const certificate = fs.readFileSync(structure.APP_MICROSERVICES_CERTIFICATE);
    const ca = [
        fs.readFileSync(structure.APP_MICROSERVICES_CA_BUNDLE_TRUST),
        fs.readFileSync(structure.APP_MICROSERVICES_CA_BUNDLE)
    ]
    const credentials = {key: privateKey, cert: certificate, ca};
    let ip = structure.APP_IP as any;
    let port = structure.APP_CONFIGANGULARWEPAGE_PORT;

    // create express app
    const app = express();
    app.use(cors());
    app.use(bodyParser.json({ limit: '150mb' }));
    app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies

    // register express routes from defined application routes
    Routes.forEach(route => {
        (app as any)[route.method](route.route, (req: Request, res: Response, next: Function) => {
            console.log(msgLog + "[index]:: METHOD = " + route.method);
            console.log(msgLog + "[index]:: ROUTE = " + route.route);
            const result = (new (route.controller as any))[route.action](req, res, next);
            console.log(msgLog + "[index]:: ACTION = " + route.action);
            if (result instanceof Promise) {
                result.then(result => result !== null && result !== undefined ? res.send(result) : undefined);
            } else if (result !== null && result !== undefined) {
                res.json(result);
            }
        });
    });

    let httpsServer = createServer(credentials,app);
    httpsServer.listen(port, ip);

    //app.listen(port,ip);

    console.log(msgLog + `[index]:: API ${structure.APP_CONFIGANGULARWEPAGE_APP_NAME} started. Open http://${ip}:${port}/`);

} catch (error) {
    console.log(error)
}
