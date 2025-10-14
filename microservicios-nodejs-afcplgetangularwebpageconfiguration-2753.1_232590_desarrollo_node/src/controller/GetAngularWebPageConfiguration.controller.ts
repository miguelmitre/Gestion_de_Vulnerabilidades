import express, { Request, Response, NextFunction } from "express";
import moment from "moment";
import structure from "../source/data";

export class GetAngularWebPageConfigurationController {

    async getInfo(request: Request, response: Response, next: NextFunction) {
        const now = new Date();
        const completeDateFormat = moment().format("yyyy-mm-dd hh:mm:ss.l");
        const msgLog = `::${process.pid}:: ${completeDateFormat} ::${structure.APP_CONFIGANGULARWEPAGE_APP_NAME}:: `;
        let result: any = { "data": {} };
        let statusCode = 200; 
        
        //encabezado HSTS
        response.setHeader("Strict-Transport-Security", "max-age=63072000; includeSubDomains");

        // Función sanitizar los datos de log
        const sanitize = (str: string) => {
            return str.replace(/[\r\n]/g, ' ').replace(/["<>]/g, '');
        };

        try {
            const sanitizedRequestBody = sanitize(JSON.stringify(request.body));
            // Comentado: Log de la solicitud
            // console.log(`${msgLog}[GetAngularWebPageConfigurationController.getInfo]:: SOLICITUD: ${sanitizedRequestBody}`);
            result.data = {
                "HOST_IMAGENES": structure.HOST_IMAGENES,
                "HOST_PDFS": structure.HOST_PDFS,
                "HOST_VIDEO": structure.HOST_VIDEO,
                "HOST_HTML": structure.HOST_HTML,
                "KEY_RECAPTCHA": structure.KEY_RECAPTCHA,
                "APY_KEY_GOOGLE_MAPS": structure.APY_KEY_GOOGLE_MAPS,
                "FONTAWESOME_INTEGRITY": structure.FONTAWESOME_INTEGRITY,
                "FONTAWESOME": structure.FONTAWESOME
            };
            // Comentado: Log de la respuesta descomentariar en caso de prueba
            // console.log(`${msgLog}[GetAngularWebPageConfigurationController.getInfo]:: RESPUESTA: ${JSON.stringify(result)}`);
        } catch (error) {
            statusCode = 500;
            // Comentado: Log de error
            // console.error(`${msgLog}[GetAngularWebPageConfigurationController.getInfo]:: ERROR: ${sanitize(JSON.stringify(error))}`);
            result = { error: "Error Interno del Servidor", details: error };
        } finally {
            if (!response.headersSent) {
                if (statusCode !== 200) {
                    response.status(statusCode).send({ error: "Hubo un problema al procesar su solicitud.", details: result });
                } else {
                    response.status(statusCode).json(result.data);
                }
            }
        }
    }
}
