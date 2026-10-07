################################################################################
# GESTION DES RÔLES EN R
# Auteur : MUHINDO KISUMBA Abdiel
# Description :
#    Système simple de gestion des utilisateurs, rôles et permissions.
#    Compatible avec R >= 4.2
################################################################################

rm(list = ls())
options(stringsAsFactors = FALSE)

###############################################################################
# BASE DES RÔLES
###############################################################################

roles <- data.frame(
  id_role = c(1,2,3,4,5),
  role = c(
    "Administrateur",
    "Manager",
    "Analyste",
    "Employe",
    "Visiteur"
  ),
  stringsAsFactors = FALSE
)

###############################################################################
# PERMISSIONS
###############################################################################

permissions <- data.frame(
  permission = c(
    "CREATE",
    "READ",
    "UPDATE",
    "DELETE",
    "EXPORT",
    "IMPORT",
    "CONFIGURATION",
    "GESTION_UTILISATEURS",
    "SAUVEGARDE"
  ),
  stringsAsFactors = FALSE
)

###############################################################################
# ASSOCIATION ROLE -> PERMISSIONS
###############################################################################

role_permissions <- list(

  Administrateur = c(
    "CREATE",
    "READ",
    "UPDATE",
    "DELETE",
    "EXPORT",
    "IMPORT",
    "CONFIGURATION",
    "GESTION_UTILISATEURS",
    "SAUVEGARDE"
  ),

  Manager = c(
    "CREATE",
    "READ",
    "UPDATE",
    "EXPORT"
  ),

  Analyste = c(
    "READ",
    "EXPORT"
  ),

  Employe = c(
    "READ"
  ),

  Visiteur = c(
    "READ"
  )

)

###############################################################################
# UTILISATEURS
###############################################################################

utilisateurs <- data.frame(

  id = 1:10,

  nom = c(
    "Abdiel",
    "Patrick",
    "Aline",
    "Jonathan",
    "Grace",
    "Claude",
    "Junior",
    "Sarah",
    "Joel",
    "Esther"
  ),

  role = c(
    "Administrateur",
    "Manager",
    "Analyste",
    "Employe",
    "Employe",
    "Manager",
    "Visiteur",
    "Analyste",
    "Employe",
    "Administrateur"
  ),

  actif = c(
    TRUE, TRUE, TRUE, TRUE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE
  ),

  stringsAsFactors = FALSE

)

###############################################################################
# FONCTION : AFFICHER UTILISATEURS
###############################################################################

afficher_utilisateurs <- function(){

  cat("\n================ UTILISATEURS ================\n")

  print(utilisateurs)

}

###############################################################################
# FONCTION : AFFICHER ROLES
###############################################################################

afficher_roles <- function(){

  cat("\n================ ROLES ================\n")

  print(roles)

}

###############################################################################
# FONCTION : LISTER PERMISSIONS D'UN ROLE
###############################################################################

permissions_role <- function(role){

  if(!role %in% names(role_permissions)){

    cat("Rôle inexistant\n")
    return(NULL)

  }

  cat("\nPermissions du rôle :", role,"\n")

  print(role_permissions[[role]])

}

###############################################################################
# FONCTION : VERIFIER UNE PERMISSION
###############################################################################

verifier_permission <- function(role, permission){

  if(!role %in% names(role_permissions))
    return(FALSE)

  permission %in% role_permissions[[role]]

}

###############################################################################
# FONCTION : AJOUTER UN UTILISATEUR
###############################################################################

ajouter_utilisateur <- function(nom, role){

  if(!role %in% roles$role){

    cat("Le rôle n'existe pas.\n")
    return()

  }

  nouveau <- data.frame(

    id = max(utilisateurs$id)+1,

    nom = nom,

    role = role,

    actif = TRUE,

    stringsAsFactors = FALSE

  )

  utilisateurs <<- rbind(utilisateurs, nouveau)

  cat("Utilisateur ajouté.\n")

}

###############################################################################
# FONCTION : CHANGER DE ROLE
###############################################################################

changer_role <- function(nom, nouveau_role){

  if(!nouveau_role %in% roles$role){

    cat("Rôle inconnu.\n")
    return()

  }

  utilisateurs$role[utilisateurs$nom==nom] <<- nouveau_role

  cat("Rôle modifié.\n")

}

###############################################################################
# FONCTION : DESACTIVER UN UTILISATEUR
###############################################################################

desactiver_utilisateur <- function(nom){

  utilisateurs$actif[utilisateurs$nom==nom] <<- FALSE

  cat("Utilisateur désactivé.\n")

}

###############################################################################
# FONCTION : ACTIVER UN UTILISATEUR
###############################################################################

activer_utilisateur <- function(nom){

  utilisateurs$actif[utilisateurs$nom==nom] <<- TRUE

  cat("Utilisateur activé.\n")

}

###############################################################################
# FONCTION : RECHERCHE
###############################################################################

rechercher_utilisateur <- function(nom){

  resultat <- utilisateurs[utilisateurs$nom==nom,]

  print(resultat)

}

###############################################################################
# FONCTION : STATISTIQUES
###############################################################################

statistiques <- function(){

  cat("\n========== NOMBRE D'UTILISATEURS PAR ROLE ==========\n")

  print(table(utilisateurs$role))

  cat("\n========== UTILISATEURS ACTIFS ==========\n")

  print(sum(utilisateurs$actif))

  cat("\n========== UTILISATEURS INACTIFS ==========\n")

  print(sum(!utilisateurs$actif))

}

###############################################################################
# FONCTION : AUDIT DES PERMISSIONS
###############################################################################

audit_permissions <- function(){

  cat("\n================ AUDIT =================\n")

  for(i in 1:nrow(utilisateurs)){

    cat(
      utilisateurs$nom[i],
      " -> ",
      utilisateurs$role[i],
      "\n"
    )

    print(role_permissions[[utilisateurs$role[i]]])

    cat("---------------------------------------\n")

  }

}

###############################################################################
# FONCTION : EXPORT CSV
###############################################################################

exporter_utilisateurs <- function(){

  write.csv(
    utilisateurs,
    "utilisateurs.csv",
    row.names = FALSE
  )

  cat("Export terminé.\n")

}

###############################################################################
# DEMONSTRATION
###############################################################################

afficher_roles()

afficher_utilisateurs()

permissions_role("Manager")

cat("\n")

cat(
  "Manager peut DELETE ? : ",
  verifier_permission("Manager","DELETE"),
  "\n"
)

cat(
  "Administrateur peut DELETE ? : ",
  verifier_permission("Administrateur","DELETE"),
  "\n"
)

ajouter_utilisateur(
  "Kevin",
  "Employe"
)

changer_role(
  "Kevin",
  "Analyste"
)

desactiver_utilisateur(
  "Joel"
)

activer_utilisateur(
  "Grace"
)

rechercher_utilisateur(
  "Kevin"
)

statistiques()

audit_permissions()

exporter_utilisateurs()

cat("\n========== FIN DU PROGRAMME ==========\n")