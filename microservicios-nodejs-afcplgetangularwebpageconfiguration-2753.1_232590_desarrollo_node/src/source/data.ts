interface data {
  APP_IP: string;
  APP_CONFIGANGULARWEPAGE_PORT: number;
  APP_CONFIGANGULARWEPAGE_APP_NAME: string;
  APP_CONFIGANGULARWEPAGE_ROUTE_GET: string;
  HOST_IMAGENES: string;
  HOST_PDFS: string;
  HOST_VIDEO: string;
  HOST_HTML: string;
  KEY_RECAPTCHA: string;
  APY_KEY_GOOGLE_MAPS: string;
  FONTAWESOME_INTEGRITY: string;
  FONTAWESOME: string;
  APP_MICROSERVICES_CERTIFICATE: string;
  APP_MICROSERVICES_KEY: string;
  APP_MICROSERVICES_CA_BUNDLE_TRUST: string;
  APP_MICROSERVICES_CA_BUNDLE: string;
}

const structure: data = { ...require("/sysx/progs/web/conf/config.json") };

export default structure;
