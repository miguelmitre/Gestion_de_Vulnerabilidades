################################################################################
#Proyecto          => SISTEMA DE AFORES( MEXICO )                              #
#Owner             => E.F.P.                                                   #
#Programa PENC914  => Carga de archivo unifica                                 #
#Fecha creacion    => 10 Feb 2010                                              #
#By                => MAGR                                                     #
################################################################################
DATABASE safre_af

GLOBALS

    DEFINE  gr_deta        RECORD
            nss                     CHAR(011),
            curp                    CHAR(018),
            rfc                     CHAR(013),
            paterno                 CHAR(040),
            materno                 CHAR(040),
            nombre                  CHAR(040),
            f_nacimiento            DATE,
            sexo                    CHAR(001),
            ent_nacimiento          CHAR(002),
            afore                   CHAR(003),
            id_procesar             CHAR(008),
            curp_icefa              CHAR(018),
            nss_icefa               CHAR(011),
            rfc_icefa               CHAR(013),
            nti                     CHAR(030),
            paterno_icefa           CHAR(040),
            materno_icefa           CHAR(040),
            nombre_icefa            CHAR(040),
            f_nacimiento_icefa      DATE,
            nombre_completo         CHAR(120),
            icefa                   CHAR(003),
            f_carga                 DATE,
            f_actualiza             DATE,
            usuario                 CHAR(008),
            correlativo             INTEGER,
            folio                   INTEGER 
            END RECORD

    DEFINE  gd_today                DATE
    DEFINE  gc_usuario              CHAR(008)
    DEFINE  gi_folio                INTEGER      
    DEFINE  gc_ruta                 LIKE safre_af:seg_modulo.ruta_rescate
    DEFINE  gc_codigo_afore         LIKE safre_af:tab_afore_local.codigo_afore

    DEFINE  enter                   CHAR(001)

DEFINE  g_reg   RECORD
        nss                     CHAR(011),
        curp                    CHAR(018),
        rfc                     CHAR(013),
        paterno                 CHAR(040),
        materno                 CHAR(040),
        nombre                  CHAR(040),
        f_nacimiento            DATE,
        sexo                    CHAR(001),
        ent_nacimiento          CHAR(002),
        afore                   CHAR(003),
        id_procesar             CHAR(008),
        curp_icefa              CHAR(018),
        nss_icefa               CHAR(011),
        rfc_icefa               CHAR(013),
        nti                     CHAR(030),
        paterno_icefa           CHAR(040),
        materno_icefa           CHAR(040),
        nombre_icefa            CHAR(040),
        f_nacimiento_icefa      DATE,
        nombre_completo         CHAR(120),
        icefa                   CHAR(003),
        f_carga                 DATE,
        f_actualiza             DATE,
        usuario                 CHAR(008),
        correlativo             INTEGER,
        folio                   INTEGER 
              END RECORD

DEFINE  g_reg5   RECORD
        curp                    CHAR(018), 
        nss                     CHAR(011)
              END RECORD

DEFINE  g_reg2   RECORD
        nss                     CHAR(011)
              END RECORD

DEFINE  g_reg3   RECORD
        curp                    CHAR(018), 
        nss                     CHAR(011)
              END RECORD

DEFINE  g_reg4   RECORD
        nss                     CHAR(011)
              END RECORD

DEFINE  g_reg1    RECORD 
        nss_ctr                 CHAR(011),
        pat_ctr                 CHAR(040),
        mat_ctr                 CHAR(040),
        nom_ctr                 CHAR(040),
        curp_afi                CHAR(018),
        nss_afi                 CHAR(011),
        fol_afi                 DECIMAL(10,0),
        sol_afi                 SMALLINT,
        pat_afi                 CHAR(040),
        mat_afi                 CHAR(040),
        nom_afi                 CHAR(040), 
        fcarga_ctr              DATE 
              END RECORD

DEFINE  g_usuario             CHAR(010)
DEFINE  g_fol_tra             INTEGER 
DEFINE  g_cuan_no             INTEGER 
DEFINE  g_status_mae_ice      SMALLINT 
DEFINE  g_icefa_cod           SMALLINT
DEFINE  g_status_afi          SMALLINT
DEFINE  g_fol                 DECIMAL(10,0)
DEFINE  g_tipo_sol            SMALLINT
DEFINE  g_validacion          CHAR(4)
DEFINE  g_fol3                DECIMAL(10,0)
DEFINE  g_tipo_sol3           SMALLINT
DEFINE  g_mont_acc            DECIMAL(16,6) 
DEFINE  g_mont_acc4           DECIMAL(16,6) 
DEFINE  g_siremp              INTEGER 
DEFINE  g_noremp              INTEGER 
DEFINE  g_noectllave          INTEGER 
DEFINE  g_nuevos              INTEGER 
DEFINE  g_status              SMALLINT 
 

END GLOBALS
