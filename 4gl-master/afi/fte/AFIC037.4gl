###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIC037  => RECEPCION ARCHIVO CONSULTA NSS CON PROCESAR         #
#Sistema           => SAFRE                                               #
#Autor             => PEDRO JIMENEZ FERRER                                #
#Fecha             => 29 DE MAYO DE 2007                                  #
#Modifico          => FERNANDO HERRERA HERNANDEZ                          #
#Fecha             => 04 DE OCTUBRE DE 2007                               #
###########################################################################
DATABASE safre_af

GLOBALS
  DEFINE 
    g_seg_modulo    RECORD LIKE seg_modulo.*,
    g_reg           RECORD
      generar       CHAR(20) 
    END RECORD, 
    g_usuario       CHAR(8),
    HOY             DATE,     
    vgenerar        CHAR(20),
    varchivo        CHAR(20),
    archb,
    arhExis         SMALLINT,
    enter           CHAR(1),
    carga           CHAR(200),
    g_plano1        INTEGER,
    g_imprime       CHAR(200),
    g_ejctaImp      CHAR(200),
    g_nmbArc        CHAR(20),
    g_nmbCga        CHAR(40),
    tot_rczdos      INTEGER,
    tot_nss         INTEGER 
END GLOBALS

MAIN

  CALL STARTLOG("AFIC037.log")

  CALL inicio()

  DEFER INTERRUPT
  OPTIONS INPUT WRAP, 
  PROMPT LINE LAST

  LET archb = 0

  CALL pantalla()

  IF archb = 1 THEN
     CALL tabTemp() 
     CALL cargaArchivo()

     IF g_plano1 >= 1 THEN
        ERROR "Procesando Informacion"
        CALL rescata_valores()
        DISPLAY g_imprime  AT 18,05
     ELSE
        ERROR "NOMBRE DE ARCHIVO INCORRECTO O VACIO, REVISE."
        SLEEP 5
        EXIT PROGRAM
     END IF
  END IF

END MAIN

FUNCTION pantalla()
#ptlla-----------------
  
  OPEN WINDOW vtna_cga AT 4,4 WITH FORM "AFIC0371" ATTRIBUTE(BORDER)
  DISPLAY " AFIC037      CARGA ARCHIVO RESPUESTA CERTIF. TRASPASOS "
  AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY HOY USING "dd-mm-yyyy" AT 3,65 ATTRIBUTE(REVERSE)

  DISPLAY g_seg_modulo.ruta_rescate AT 7,10

  INPUT BY NAME g_reg.generar
    AFTER FIELD generar
      IF g_reg.generar IS NULL THEN
         ERROR "Campo NO puede ser NULO"
         NEXT FIELD generar
      ELSE
         LET vgenerar = g_reg.generar 

         {CALL valiExten()
         IF archb = 0 THEN
            PROMPT "Archivo Correcto <ENTER> para continuar, 
                    No < Ctrl-C > ." FOR enter
            NEXT FIELD generar
         ELSE}
         
         CALL arhExiste()
         IF arhExis > 0 THEN
            ERROR "ARCHIVO YA PROCESADO"
            SLEEP 1
            ERROR " "
            INITIALIZE g_reg.generar, vgenerar TO NULL
            LET archb = 0
            CLEAR FORM
            NEXT FIELD generar
         ELSE
            {LET g_nmbCga = "PAFFT.DP.A568.E01.",vgenerar CLIPPED,
                           ".C001.RCONSULT"}
            LET g_nmbCga = vgenerar CLIPPED
            
            DISPLAY  g_nmbCga AT 10,10 
            
            PROMPT "El Archivo es correcto (S/N) : " FOR enter
 
            IF enter = "s" OR enter = "S" THEN
               LET archb = 1
               EXIT INPUT
            ELSE
               ERROR " "
               DISPLAY "                   ",
                       "                   "  AT 10,10 
               INITIALIZE g_reg.generar, vgenerar TO NULL
               LET archb = 0
               CLEAR FORM
               NEXT FIELD generar
            END IF
         END IF

         #END IF
      END IF
   END INPUT

END FUNCTION

FUNCTION rescata_valores()
#rcv---------------------
  DEFINE 
    rg          RECORD
                nss                CHAR(11),
                tipo_cta           CHAR(02),
                fler               CHAR(39),
                rfc                CHAR(13),
                curp               CHAR(18),
                paterno            CHAR(40),
                materno            CHAR(40),
                nombre             CHAR(40),
                fec_ncmto          CHAR(08),
                sexo               CHAR(10),
                ent_ncmnto         CHAR(20),
                fol_soli           CHAR(10),
                nom_afore          CHAR(40),
                cve_prmtor         CHAR(10),
                fec_prmer_afili    CHAR(08),
                fec_ultma_actu     CHAR(08),
                fec_alta_afore     CHAR(08),
                ultmo_sali_d_i     CHAR(05),
                ultmo_peri_pgo     CHAR(06),
                fec_recepcion      CHAR(08),
                hr_recepcion       CHAR(08),
                conse_recpcn       CHAR(08),
                nomb_trab_imss     CHAR(50),
                fal_procanase      CHAR(08),
                fec_ultm_mov       CHAR(08),
                sexo_imss          CHAR(10),
                mes_nacimiento     CHAR(02),
                ent_ncmnto_ims     CHAR(20),
                afore_asignada     CHAR(40),
                fecha_asignada     CHAR(08),
                nomb_prsta_serv    CHAR(40),
                fec_alta_ps        CHAR(08),
                mtvo_rchso01       CHAR(03),   
                mtvo_rchso02       CHAR(03),   
                mtvo_rchso03       CHAR(03),   
                resul_val_trans    CHAR(03),
                indcdorValApBd     CHAR(01),
                indcdorValAmBd     CHAR(01),
                indcdorValNbBd     CHAR(01),
                indcdorValApPc     CHAR(01),
                indcdorValAmPc     CHAR(01),
                indcdorValNbPc     CHAR(01),
                filer02            CHAR(06)
                END RECORD,

    folio       DECIMAL(16,0),
    tsoli       SMALLINT,
    sta_inter   SMALLINT,
    cod_oper    CHAR(2),
    dig_pcso    CHAR(15),
    fmvto       DATE,
    hora        CHAR(08),
    process     CHAR(30),
    digimage    CHAR(30),
    vallogica   CHAR(30),
    clasifdoc   CHAR(30),
    document    CHAR(30),
    certific    CHAR(30),
    arh         CHAR(30)
         

  LET tot_rczdos = 0
  LET tot_nss    = 0  
  LET g_imprime  = g_seg_modulo.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                   ".rechazados",fmvto USING "DDMMYYYY","_",hora CLIPPED

  START REPORT rpt_rchzos TO g_imprime
   
  DECLARE cur_tnspso CURSOR FOR
  SELECT n_reg[1,11],   n_reg[12,13],  n_reg[14,52],  n_reg[53,65],
         n_reg[66,83],  n_reg[84,123], n_reg[124,163],n_reg[164,203],
         n_reg[204,211],n_reg[212,221],n_reg[222,241],n_reg[242,251],
         n_reg[252,291],n_reg[292,301],n_reg[302,309],n_reg[310,317],
         n_reg[318,325],n_reg[326,332],n_reg[333,338],n_reg[339,346],
         n_reg[347,354],n_reg[355,362],n_reg[363,412],n_reg[413,420],
         n_reg[421,428],n_reg[429,438],n_reg[439,440],n_reg[441,460],
         n_reg[461,500],n_reg[501,508],n_reg[509,548],n_reg[549,556],
         n_reg[557,559],n_reg[560,562],n_reg[563,565],n_reg[566,568],
         n_reg[569,569],n_reg[570,570],n_reg[571,571],n_reg[572,572],
         n_reg[573,573],n_reg[574,574],n_reg[575,580]
  FROM   safre_tmp:planoA
  FOREACH cur_tnspso INTO rg.nss,            rg.tipo_cta,       rg.fler,            rg.rfc,
                          rg.curp,           rg.paterno,        rg.materno,         rg.nombre,
                          rg.fec_ncmto,      rg.sexo,           rg.ent_ncmnto,      rg.fol_soli, 
                          rg.nom_afore,      rg.cve_prmtor,     rg.fec_prmer_afili, rg.fec_ultma_actu,  
                          rg.fec_alta_afore, rg.ultmo_sali_d_i, rg.ultmo_peri_pgo,  rg.fec_recepcion,
                          rg.hr_recepcion,   rg.conse_recpcn,   rg.nomb_trab_imss,  rg.fal_procanase,
                          rg.fec_ultm_mov,   rg.sexo_imss,      rg.mes_nacimiento,  rg.ent_ncmnto_ims,
                          rg.afore_asignada, rg.fecha_asignada, rg.nomb_prsta_serv, rg.fec_alta_ps,
                          rg.mtvo_rchso01,   rg.mtvo_rchso02,   rg.mtvo_rchso03,    rg.resul_val_trans,
                          rg.indcdorValApBd, rg.indcdorValAmBd, rg.indcdorValNbBd,  rg.indcdorValApPc,
                          rg.indcdorValAmPc, rg.indcdorValNbPc, rg.filer02

    CALL getFoliSoli(rg.nss)RETURNING folio,tsoli

    IF folio > 0 THEN
       CALL rechazos(rg.nss,folio,tsoli,rg.mtvo_rchso01, 
                     rg.mtvo_rchso02, rg.mtvo_rchso03,
                     rg.resul_val_trans, rg.fec_prmer_afili,
                     rg.ultmo_sali_d_i, rg.ultmo_peri_pgo, 
                     rg.fec_recepcion, rg.hr_recepcion,
                     rg.conse_recpcn, rg.fecha_asignada, 
                     rg.indcdorValApBd, rg.indcdorValAmBd,
                     rg.indcdorValNbBd)
                     RETURNING sta_inter,cod_oper,dig_pcso

       IF dig_pcso != '00' THEN
          CALL traeStam(folio,tsoli) RETURNING process,digimage,vallogica,
                                               clasifdoc,document,certific,
                                               arh
          CALL actlzaStm(folio,tsoli)

          OUTPUT TO REPORT rpt_rchzos(folio,rg.nss,dig_pcso,process,
                                      digimage,vallogica,
                                      clasifdoc,document,certific,arh)
          LET tot_rczdos = tot_rczdos + 1
       END IF 

       {INSERT INTO afi_nss_consulta VALUES(rg.nss,folio,tsoli,fmvto,sta_inter,
                                           cod_oper,dig_pcso,g_usuario)}
       UPDATE afi_nss_consulta
          SET status_interno = sta_inter,
              cod_operacion  = cod_oper,
              diag_proceso   = dig_pcso
        WHERE nss            = rg.nss
          AND n_folio        = folio
          AND tipo_solicitud = tsoli
    END IF

    LET tot_nss = tot_nss + 1

  END FOREACH

  CALL actlzaArc()

  DISPLAY " Total NSS CARGADOS             : ", 
          tot_nss USING "###,###,###" AT 15,08 

  DISPLAY " Total de de  FOLIOS RECHAZADOS : ", 
          tot_rczdos USING "###,###,###" AT 17,08 

  FINISH REPORT rpt_rchzos

  LET g_ejctaImp = "lp ",g_imprime

  RUN g_ejctaImp

END FUNCTION

FUNCTION cargaArchivo()
#ca-------------------
   LET carga = NULL
   LET carga = g_seg_modulo.ruta_rescate CLIPPED,"/",g_nmbCga CLIPPED

   WHENEVER ERROR CONTINUE
      LOAD FROM carga INSERT INTO safre_tmp:planoA
   WHENEVER ERROR STOP

   LET g_plano1 = 0 
   SELECT COUNT(*) INTO   g_plano1
   FROM   safre_tmp:planoA
   IF g_plano1 IS NULL THEN 
      LET g_plano1 = 0 
   END IF

END FUNCTION

FUNCTION valiExten()
#vca-----------------
  DEFINE longitud,i,i2   SMALLINT 

  LET longitud = LENGTH(g_reg.generar CLIPPED)

  FOR i  = 1 TO longitud
    IF g_reg.generar[i] = "." THEN
       LET i  = i + 1
       LET i2 = i + 2
       IF g_reg.generar[i,i2] = 'afi' THEN
          LET g_reg.generar = g_reg.generar[i,i2]
          LET archb = 1
          EXIT FOR
       ELSE
          LET archb = 0
          EXIT FOR
       END IF
    END IF
  END FOR

END FUNCTION

FUNCTION arhExiste()
#vae-----------------

  LET arhExis = 0

  SELECT COUNT(*) 
    INTO arhExis 
    FROM afi_ctr_arh_reg
   WHERE @nombre_archivo = vgenerar
  IF arhExis IS NULL THEN
     LET arhExis = 0
  END IF 

END FUNCTION

FUNCTION actlzaArc()
#acta----------------
  DEFINE fhoy DATE,
         hoy  CHAR(10)
  
  LET vgenerar = vgenerar CLIPPED
  LET fhoy     = TODAY
  LET hoy      = fhoy USING "MM/DD/YYYY"

  INSERT INTO afi_ctr_arh_reg VALUES(vgenerar,0,tot_rczdos,0,0,tot_nss,hoy)

END FUNCTION

FUNCTION inicio()
#ini-------------

  SELECT *, USER INTO g_seg_modulo.*, g_usuario
    FROM  seg_modulo
   WHERE  modulo_cod = 'afi'

END FUNCTION

FUNCTION tabTemp() 
#tmp--------------

  WHENEVER ERROR CONTINUE
    DATABASE safre_tmp
    DROP TABLE safre_tmp:planoA

    CREATE TABLE safre_tmp:planoA
      (n_reg CHAR(580))

  WHENEVER ERROR STOP

  DATABASE safre_af

END FUNCTION

FUNCTION  getFoliSoli(nss)
#gFS---------------------
  DEFINE nss   CHAR(11),
         fol   DECIMAL(16,0),
         tsoli SMALLINT

  LET fol = 0

  SELECT UNIQUE n_folio INTO fol 
    FROM afi_nss_consulta
   WHERE nss            = nss
     AND status_interno = 30
  IF fol IS NULL THEN
     LET fol = 0
  END IF

  SELECT UNIQUE tipo_solicitud INTO tsoli
    FROM afi_nss_consulta
   WHERE nss            = nss
     AND n_folio        = fol
     AND status_interno = 30
  IF tsoli IS NULL THEN
     LET tsoli = 0
  END IF

  RETURN fol,tsoli

END FUNCTION

FUNCTION rechazos(nss,fol,tpSoli,mtvo_rchso01,mtvo_rchso02,
                  mtvo_rchso03,resul_val_trans,fec_prmer_afili,
                  ultmo_sali_d_i,ultmo_peri_pgo,fec_recepcion,
                  hr_recepcion,conse_recpcn,fecha_asignada,
                  indcdorValApBd,indcdorValAmBd,indcdorValNbBd)
#r-------------------------------------------------------------
  DEFINE nss                CHAR(11),
         fol                DECIMAL(16,0),
         tpSoli             SMALLINT,
         mtvo_rchso01       CHAR(03),   
         mtvo_rchso02       CHAR(03),   
         mtvo_rchso03       CHAR(03),   
         resul_val_trans    CHAR(03),   
         fec_prmer_afili    CHAR(08),
         ultmo_sali_d_i     CHAR(05),
         ultmo_peri_pgo     CHAR(06),
         fec_recepcion      CHAR(08),
         hr_recepcion       CHAR(08),
         conse_recpcn       CHAR(08),
         fecha_asignada     CHAR(08),
         sta_inter          SMALLINT,
         cod_oper           CHAR(2),
         dig_pcso           CHAR(15),
         x                  SMALLINT,
         mtvo_rchzo         CHAR(03),   
         a_num              SMALLINT,
         fecha_asig         DATE,
         fecCalcu           CHAR(10),
         fec_asig_calcu     DATE,
         nvoTpSol           SMALLINT,
         indcdorValApBd     CHAR(01),
         indcdorValAmBd     CHAR(01),
         indcdorValNbBd     CHAR(01)

 LET dig_pcso       = '00'
 LET a_num          = fecha_asignada[1,4]
 LET a_num          = a_num + 1
 LET fecCalcu       = fecha_asignada[7,8],'/',fecha_asignada[5,6],'/',a_num 
 LET fec_asig_calcu = fecCalcu
 LET fecha_asig     = TODAY

 IF fec_asig_calcu > fecha_asig THEN
    #CALL traeTipoSoli(nss,fol)RETURNING nvoTpSol
    
    #IF nvoTpSol = 27 THEN
    IF tpSoli = 2 THEN
       UPDATE solicitudafi 
       SET    idtposolicitud = 26
       WHERE  nsolicitud     = fol
       AND    idtposolicitud = 27
       AND    idpersona = (SELECT UNIQUE idpersona
                           FROM   datosacceso
                           WHERE  valor = nss 
                           AND   (dat1.idtpoidnalterno IS NULL    
                           OR     dat1.idtpoidnalterno IN (SELECT idtpoidnalterno
                                                           FROM   tpoidnalterno
                                                           WHERE  cvetpoidnalterno = 'NSS')))

       UPDATE solicitudafi 
       SET     idtposolicitud = 26  
       WHERE   nsolicitud     = fol
       AND     idtposolicitud = 27

       LET dig_pcso = 'G9'
    ELSE
       IF tpSoli = 1  THEN     #nvoTpSol = 26 THEN
          LET dig_pcso = 'G10'       
       {ELSE
          LET dig_pcso = 'G9'}
       END IF
    END IF

    LET sta_inter = 40 
    LET cod_oper  = '02'
                     
    RETURN sta_inter,cod_oper,dig_pcso 

 END IF
 
 IF tpSoli = 2 THEN
    IF indcdorValApBd = "0" OR 
       indcdorValAmBd = "0" OR 
       indcdorValNbBd = "0" THEN

       LET dig_pcso  = 'G16'
       LET sta_inter = 40 
       LET cod_oper  = '02'
   
       RETURN sta_inter,cod_oper,dig_pcso
    END IF
 END IF

 IF tpSoli = 2 THEN
    IF fec_prmer_afili IS NULL OR fec_prmer_afili != '        ' THEN
       IF ultmo_sali_d_i IS NOT NULL  OR ultmo_sali_d_i != '    ' THEN 
         IF ultmo_peri_pgo IS NOT NULL OR ultmo_peri_pgo != '     ' THEN
            LET dig_pcso  = 'G12'
            LET sta_inter = 40 
            LET cod_oper  = '02'
              
            RETURN sta_inter,cod_oper,dig_pcso

         END IF
       END IF
    END IF  
 END IF

 IF fec_recepcion IS NOT NULL OR  fec_recepcion != '        ' THEN
    IF hr_recepcion IS NOT NULL OR hr_recepcion != '        ' THEN
       IF conse_recpcn IS NOT NULL OR conse_recpcn != '        ' THEN 
         LET dig_pcso  = 'G11'
         LET sta_inter = 40 
         LET cod_oper  = '02'
                     
         RETURN sta_inter,cod_oper,dig_pcso

       END IF
    END IF
 END IF


 FOR x = 1 TO 4
    CASE x
      WHEN 1 LET mtvo_rchzo = mtvo_rchso01
      WHEN 2 LET mtvo_rchzo = mtvo_rchso02
      WHEN 3 LET mtvo_rchzo = mtvo_rchso03
      WHEN 4 LET mtvo_rchzo = resul_val_trans
    END CASE

    CASE mtvo_rchzo
      WHEN '001' LET dig_pcso  = 'G6f'
      WHEN '005' LET dig_pcso  = 'G6c'
      WHEN '015' LET dig_pcso  = 'G6b'
      WHEN '528' LET dig_pcso  = 'G6a'
      WHEN '597' LET dig_pcso  = 'G6d'
      WHEN '598' LET dig_pcso  = 'G6e'
      WHEN '793' LET dig_pcso  = 'G10'
      OTHERWISE  LET dig_pcso  = '00'
    END CASE

     ##### Son informativos y no se rechazan.
     # WHEN '002'
     #    LET dig_pcso  = '/'
     # WHEN '003'
     #    LET dig_pcso  = ''
     # WHEN '004'
     #    LET dig_pcso  = ''
     # WHEN '030'
     #    LET dig_pcso  = ''
     # WHEN '054'
     #    LET dig_pcso  = ''
     # WHEN '059'
     #    LET dig_pcso  = ''
     ##### Son informativos

    IF dig_pcso != '00' THEN
       EXIT FOR
    END IF 
 END FOR

 IF dig_pcso = '00' THEN
    LET sta_inter = 100 
    LET cod_oper  = '01'
 ELSE
    LET sta_inter = 40 
    LET cod_oper  = '02'  
 END IF

 RETURN sta_inter,cod_oper,dig_pcso

END FUNCTION

FUNCTION traeTipoSoli(nss,fol)
  DEFINE nss CHAR(11),
         fol DECIMAL(16,0),
         tpSol SMALLINT

  SELECT UNIQUE idtposolicitud  INTO tpSol
    FROM solicitudafi
   WHERE nsolicitud = fol
     AND idpersona  = (SELECT UNIQUE idpersona
                         FROM datosacceso
                        WHERE valor = nss 
                          AND (dat1.idtpoidnalterno IS NULL
                           OR  dat1.idtpoidnalterno IN (SELECT idtpoidnalterno
                                                        FROM   tpoidnalterno
                                                        WHERE  cvetpoidnalterno = 'NSS'))) 

  RETURN tpSol
END FUNCTION

FUNCTION traeStam(fol,tsoli)
  DEFINE fol       DECIMAL(16,0),
         tsoli     SMALLINT,
         process   CHAR(30),
         digimage  CHAR(30),
         vallogica CHAR(30),
         clasifdoc CHAR(30),
         document  CHAR(30),
         certific  CHAR(30),
         arh       CHAR(30)

  SELECT stamprocessafi,stamdigimageafi,stamresulvallogica,
         stamresulclasifdoc,stamdocumentafi,stamcertificafi,
         stamfile
    INTO process,digimage,vallogica,clasifdoc,document,
         certific,arh
    FROM solicitudafi
   WHERE idtposolicitud = tsoli
     AND nsolicitud     = folio

 RETURN process,digimage,vallogica,clasifdoc,document,
        certific,arh

END FUNCTION 

FUNCTION actlzaStm(fol,tsoli)
  DEFINE fol   DECIMAL(16,0),
         tsoli SMALLINT

  UPDATE solicitudafi 
     SET stamprocessafi     = 'Revisada Documentos',
         stamresulvallogica = 'Rechazada ValDoc',
         stamcertificafi    = '5-Rechazada x Proceso'
   WHERE nsolicitud         = fol
     AND idtposolicitud     = tsoli

END FUNCTION

REPORT rpt_rchzos(folio,nss,dig_pcso,process,digimage,
                  vallogica,clasifdoc,document,certific,arh)
#rpt----------------------------------------
  DEFINE folio     DECIMAL(16,0),
         nss       CHAR(11),
         dig_pcso  CHAR(15),
         process   CHAR(30),
         digimage  CHAR(30),
         vallogica CHAR(30),
         clasifdoc CHAR(30),
         document  CHAR(30),
         certific  CHAR(30),
         arh       CHAR(30)
  OUTPUT
    LEFT MARGIN 0
    RIGHT MARGIN 0
    TOP MARGIN 0
    BOTTOM MARGIN 0
    PAGE LENGTH 66

  FORMAT

  ON EVERY ROW

    PRINT COLUMN  01, folio,nss,dig_pcso,process,
                      digimage,vallogica,clasifdoc,document,
                      certific,arh

END REPORT

