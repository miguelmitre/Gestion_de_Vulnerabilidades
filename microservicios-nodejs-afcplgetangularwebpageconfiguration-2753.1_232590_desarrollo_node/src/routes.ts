import { GetAngularWebPageConfigurationController } from "./controller/GetAngularWebPageConfiguration.controller";
import structure from "./source/data";

export const Routes = [
    {
        method: "get",
        route: structure.APP_CONFIGANGULARWEPAGE_ROUTE_GET,
        controller: GetAngularWebPageConfigurationController,
        action: "getInfo"
    }
];