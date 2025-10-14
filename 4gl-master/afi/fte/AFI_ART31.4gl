##############################################################################
#Proyecto            => SAFRE (Mexico)                                       #
#Propietario         => E.F.P.                                               #
#Programa AFI_ART31  => FORMATO REQUERIMIENTO INFORMACION ART. 31.           #
#Fecha actualiz.     => 17 MARZO 2005                                        #
#Actualizacion       => FERNANDO HERRERA HERNANDEZ                           #
#Fecha actualiz.     =>                                                      #
#Sistema             => AFI                                                  #
##############################################################################
DATABASE safre_af
GLOBALS

   DEFINE 
     enter                 CHAR(01),
     id61                  CHAR(40),
     id62                  CHAR(01), 
     id63                  DATE    ,
     id78                  DATE    ,
     id83                  CHAR(01),
     reg_mae               RECORD LIKE afi_mae_afiliado.*,

     #var id61
     vrowid                INTEGER,

     #var id62
     vfactualiza           DATE

END GLOBALS
##############################################################################
MAIN
  
  DEFER INTERRUPT
  OPTIONS
     PROMPT LINE LAST
    
  CALL inicio()
  CALL proceso_principal()

END MAIN
##############################################################################
FUNCTION inicio()

  WHENEVER ERROR CONTINUE
        DATABASE safre_tmp
        DROP TABLE safre_tmp:tmp_afi_art31
  WHENEVER ERROR STOP

  CREATE TABLE safre_tmp:tmp_afi_art31
       (nss                 CHAR(11)     ,
        n_folio          DECIMAL(10,0) ,
        tipo_solicitud      SMALLINT     ,
        correo_electronico  CHAR(40)     ,
        marca_domicilio     CHAR(1)      ,
        fecha_nacimiento    DATE         ,
        fecha_certificacion DATE         ,
        marca_credito       CHAR(1)
        );
 
   DATABASE safre_af

   SELECT b.n_seguro
     FROM acr_det_cedido a, acr_det_devuelto b
    WHERE a.nss_afore          = b.n_seguro
      AND a.fecha_trasp        < b.fecha_mov_banxico
      AND a.estado             = 0
      AND a.tipo_transferencia = '03'
   INTO TEMP tmp_acr_devuelto

   LET id61 = NULL
   LET id62 = 0
   LET id83 = 0

END FUNCTION
##############################################################################
FUNCTION proceso_principal()

    PROMPT "Desea generar el proceso[S/N]: " FOR enter

    IF enter MATCHES "[sS]" THEN

       ERROR "Procesando información. . ."

       DECLARE cur_1 CURSOR FOR
       SELECT a.*
         FROM afi_mae_afiliado a, afi_ctas_siefore b
        WHERE a.n_seguro = b.nss
       FOREACH cur_1 INTO reg_mae.*
        
          SELECT MAX(rowid), telefono 
            INTO vrowid, id61
            FROM afi_telefono
           WHERE tel_cod        = 7
             AND n_folio        = reg_mae.n_folio
             AND tipo_solicitud = reg_mae.tipo_solicitud
             AND nss            = reg_mae.n_seguro
           GROUP BY 2

          SELECT factualiza
            INTO vfactualiza
            FROM afi_domicilio
           WHERE marca_envio = 'X'
          IF vfactualiza > reg_mae.fentcons THEN
             LET id62 = 1
          ELSE
             LET id62 = 0
          END IF
    
          LET id63 = reg_mae.fena
          LET id78 = reg_mae.fentcons
    
          SELECT 'X'
            FROM acr_det_cedido 
           WHERE nss_afore = reg_mae.n_seguro
             AND NOT EXISTS (SELECT n_seguro 
                               FROM tmp_acr_devuelto)
             AND estado = 0
             AND fecha_trasp <= '01142005'
          IF STATUS = NOTFOUND THEN
             LET id83 = 1 
          ELSE 
             #LET id83 = 0
          #END IF
    
             #Falta marca 237
             SELECT 'X'
               FROM cta_act_marca
              WHERE nss = reg_mae.n_seguro
                AND marca_cod = 237
             GROUP BY 1
             IF STATUS = NOTFOUND THEN
                SELECT 'X'
                  FROM cta_his_marca
                 WHERE nss = reg_mae.n_seguro
                   AND marca_cod = 230
                IF STATUS = NOTFOUND THEN
                   LET id83 = 1 
                ELSE 
                   LET id83 = 0
                END IF
             ELSE
                LET id83 = 0
             END IF 
          END IF
    
          INSERT INTO safre_tmp:tmp_afi_art31 VALUES
          (reg_mae.n_seguro, reg_mae.n_folio, reg_mae.tipo_solicitud,
           id61, id62, id63, id78, id83)
   
          LET id61 = NULL
          LET id62 = 0
          LET id83 = 0
          
       END FOREACH

       PROMPT "Procesando finalizado, presione <ENTER> para salir." FOR enter

    ELSE

       ERROR "Proceso abortado."
       SLEEP 2
       EXIT PROGRAM

    END IF

END FUNCTION
##############################################################################
