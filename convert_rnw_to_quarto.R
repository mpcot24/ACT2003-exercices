# ============================================================
# convert_rnw_to_quarto.R
# Conversion LaTeX/Sweave (.Rnw) → Quarto (.qmd)
# Un fichier .qmd par exercice + fichier chapitre agrégateur
# ============================================================
#
# USAGE :
#   1. Ajuster DOSSIER_SOURCE et DOSSIER_SORTIE ci-dessous
#   2. source("convert_rnw_to_quarto.R")
#
# STRUCTURE PRODUITE :
#   chapitres/
#     chap_base/
#       ex_ech_alea_oui_non.qmd
#       ex_dans_chacun_des_cas.qmd
#       ...
#     chap_base.qmd          ← agrège via {{< include >}}
#   _label_map.rds            ← pour débogage
# ============================================================

library(stringr)
library(fs)

# ── Configuration ──────────────────────────────────────────────────────────────

DOSSIER_SOURCE <- "old_rnw/" # dossier contenant les .Rnw
DOSSIER_SORTIE <- "chapitres/" # dossier de sortie

# ── 1. Utilitaires de base ─────────────────────────────────────────────────────

# Générer un slug lisible depuis le début du texte LaTeX (fallback si pas de \filename)
make_slug <- function(texte, max_mots = 6) {
    texte |>
        str_replace_all("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1") |>
        str_replace_all("[^a-zA-ZÀ-ÿ0-9 ]", " ") |>
        str_squish() |>
        str_to_lower() |>
        iconv(to = "ASCII//TRANSLIT") |>
        str_replace_all("[^a-z0-9 ]", "") |>
        str_split(" ") |>
        (\(x) x[[1]])() |>
        (\(mots) mots[nchar(mots) > 2][seq_len(min(max_mots, sum(nchar(mots) > 2)))])() |>
        paste(collapse = "_")
}

# Garantir l'unicité des slugs dans un vecteur
slugs_uniques <- function(slugs) {
    vus <- list()
    for (i in seq_along(slugs)) {
        base <- slugs[i]
        if (is.null(vus[[base]])) {
            vus[[base]] <- 1L
        } else {
            vus[[base]] <- vus[[base]] + 1L
            slugs[i] <- paste0(base, "_", vus[[base]])
        }
    }
    slugs
}

# Extraire \filename{} et \title{} du début d'un bloc exercice
extraire_meta_exercice <- function(inner) {
    filename <- str_match(inner, "\\\\filename\\{([^}]+)\\}")[, 2]
    titre <- str_match(inner, "\\\\title\\{([^}]+)\\}")[, 2]
    # Supprimer ces deux commandes du texte de l'énoncé
    inner_propre <- inner |>
        str_replace("\\\\filename\\{[^}]+\\}\\s*", "") |>
        str_replace("\\\\title\\{[^}]+\\}\\s*", "")
    list(
        filename = if (!is.na(filename)) filename else NULL,
        titre    = if (!is.na(titre)) titre else NULL,
        inner    = inner_propre
    )
}

# ── 2. Transformations LaTeX → Markdown ────────────────────────────────────────

convertir_environnements_math <- function(txt) {
    txt |>
        str_replace_all("\\\\begin\\{align\\*\\}", "$$\n\\\\begin{aligned}") |>
        str_replace_all("\\\\end\\{align\\*\\}", "\\\\end{aligned}\n$$") |>
        str_replace_all("\\\\begin\\{align\\}", "$$\n\\\\begin{aligned}") |>
        str_replace_all("\\\\end\\{align\\}", "\\\\end{aligned}\n$$") |>
        str_replace_all("\\\\begin\\{equation\\*\\}", "$$") |>
        str_replace_all("\\\\end\\{equation\\*\\}", "$$") |>
        str_replace_all("\\\\begin\\{equation\\}", "$$") |>
        str_replace_all("\\\\end\\{equation\\}", "$$") |>
        str_replace_all("\\\\begin\\{displaymath\\}", "$$") |>
        str_replace_all("\\\\end\\{displaymath\\}", "$$") |>
        # \intertext{} → sortir du bloc align
        str_replace_all(
            "\\\\intertext\\{([^}]*)\\}",
            "\\\\end{aligned}\n$$\n\n\\1\n\n$$\n\\\\begin{aligned}"
        )
}

convertir_listes <- function(lignes) {
    # Traitement ligne par ligne pour gérer l'imbrication et la numérotation
    resultat <- character()
    dans_liste <- FALSE
    compteur <- 1L
    buffer <- ""

    vider_buffer <- function() {
        if (nchar(trimws(buffer)) > 0) {
            resultat <<- c(resultat, trimws(buffer), "")
            buffer <<- ""
        }
    }

    for (l in lignes) {
        if (grepl("\\\\begin\\{(enumerate|inparaenum|itemize)\\}", l)) {
            dans_liste <- TRUE
            compteur <- 1L
            buffer <- ""
        } else if (grepl("\\\\end\\{(enumerate|inparaenum|itemize)\\}", l)) {
            vider_buffer()
            dans_liste <- FALSE
        } else if (dans_liste && grepl("\\\\item", l)) {
            vider_buffer()
            prefixe <- paste0("**", letters[compteur], ")** ")
            buffer <- str_replace(l, "\\\\item\\s*", prefixe)
            compteur <- compteur + 1L
        } else if (dans_liste) {
            propre <- trimws(l)
            if (nchar(propre) > 0) buffer <- paste(buffer, propre)
        } else {
            resultat <- c(resultat, l)
        }
    }
    if (nchar(trimws(buffer)) > 0) resultat <- c(resultat, buffer)
    resultat
}

convertir_chunks_sweave <- function(txt) {
    # Convertir <<opts>>= ... @ → ```{r} ... ```
    lignes <- str_split(txt, "\n")[[1]]
    out <- character()
    i <- 1L

    while (i <= length(lignes)) {
        l <- lignes[i]

        if (str_detect(l, "^\\s*<<")) {
            opts <- str_replace_all(l, "<<|>>=", "") |> str_trim()

            # Options Quarto
            yaml <- ""
            if (str_detect(opts, "echo\\s*=\\s*FALSE")) yaml <- paste0(yaml, "#| echo: false\n")
            if (str_detect(opts, "eval\\s*=\\s*FALSE")) yaml <- paste0(yaml, "#| eval: false\n")
            if (str_detect(opts, "fig\\s*=\\s*TRUE")) yaml <- paste0(yaml, "#| fig-show: asis\n")
            if (str_detect(opts, "out\\.width")) {
                w <- str_match(opts, "out\\.width\\s*=\\s*\"([^\"]+)\"")[, 2]
                if (!is.na(w)) yaml <- paste0(yaml, "#| out-width: \"", w, "\"\n")
            }

            # Identifiant de chunk si nommé
            nom <- str_match(opts, "^([a-zA-Z][a-zA-Z0-9._-]*)")[, 2]
            header <- if (!is.na(nom) && nom != "") {
                paste0("```{r ", nom, "}")
            } else {
                "```{r}"
            }

            out <- c(out, header)
            if (nchar(yaml) > 0) out <- c(out, str_trim(yaml))
            out <- c(out, "")

            i <- i + 1L
            while (i <= length(lignes) && !str_detect(lignes[i], "^\\s*@\\s*$")) {
                out <- c(out, lignes[i])
                i <- i + 1L
            }
            out <- c(out, "```", "")
        } else {
            out <- c(out, l)
        }
        i <- i + 1L
    }
    paste(out, collapse = "\n")
}

latex_vers_md <- function(txt) {
    # Appliquer toutes les transformations textuelles
    txt |>
        str_replace_all("\\\\mbox\\{([^}]*)\\}", "\\\\text{\\1}") |>
        str_replace_all("\\\\emph\\{([^}]*)\\}", "*\\1*") |>
        str_replace_all("\\\\textbf\\{([^}]*)\\}", "**\\1**") |>
        str_replace_all("\\\\textit\\{([^}]*)\\}", "*\\1*") |>
        str_replace_all("\\\\emph\\{([^}]*)\\}", "*\\1*") |>
        str_replace_all("~", "\u00a0") |>
        str_replace_all("\\\\,", "\u202f") |>
        str_replace_all("\\\\noindent\\s*", "") |>
        str_replace_all("\\\\bigskip\\s*", "\n\n") |>
        str_replace_all("\\\\medskip\\s*", "\n") |>
        str_replace_all("\\\\caption\\{([^}]*)\\}", ": \\1") |>
        str_replace_all("\\\\begin\\{figure\\}[^\\n]*", "") |>
        str_replace_all("\\\\end\\{figure\\}", "") |>
        str_replace_all("\\\\centering\\s*", "") |>
        str_replace_all("(?m)%[^\n]*", "") |> # commentaires LaTeX
        str_replace_all("\n{3,}", "\n\n") |>
        str_trim()
}

# Pipeline complet pour un bloc de texte
transformer_bloc <- function(txt) {
    # 1. Chunks Sweave
    txt <- convertir_chunks_sweave(txt)
    # 2. Listes (ligne par ligne)
    lignes <- str_split(txt, "\n")[[1]]
    lignes <- convertir_listes(lignes)
    txt <- paste(lignes, collapse = "\n")
    # 3. Maths
    txt <- convertir_environnements_math(txt)
    # 4. Texte général
    txt <- latex_vers_md(txt)
    txt
}

# ── 3. Gestion des labels et références croisées ───────────────────────────────

# Extraire tous les \label{} d'un texte → vecteur de clés
extraire_labels <- function(txt) {
    m <- str_extract_all(txt, "\\\\label\\{([^}]+)\\}")[[1]]
    str_match(m, "\\\\label\\{([^}]+)\\}")[, 2]
}

# Supprimer les \label{} du texte (après les avoir enregistrés)
supprimer_labels <- function(txt) {
    str_replace_all(txt, "\\\\label\\{[^}]+\\}", "")
}

# Remplacer les \ref{} par des liens Quarto en utilisant la table de correspondance
remplacer_refs <- function(txt, label_map) {
    refs <- str_extract_all(txt, "\\\\ref\\{[^}]+\\}")[[1]] |> unique()
    for (ref in refs) {
        cle <- str_match(ref, "\\\\ref\\{([^}]+)\\}")[, 2]
        if (!is.null(label_map[[cle]])) {
            remplacement <- paste0("@", label_map[[cle]])
        } else {
            remplacement <- paste0("**[réf:", cle, "]**")
            message("  ⚠ Référence non résolue : ", cle)
        }
        txt <- str_replace_all(txt, fixed(ref), remplacement)
    }
    txt
}

# ── 4. Extraction des éléments spéciaux ────────────────────────────────────────

# Extraire et supprimer les indices ([Indices: ...]) de l'énoncé
extraire_indices <- function(enonce) {
    m <- str_match(
        enonce,
        "(?s)\\[Indices?\\s*:\\s*(\\\\begin\\{itemize\\}.*?\\\\end\\{itemize\\})\\s*\\]"
    )
    if (!is.na(m[1, 1])) {
        bloc <- m[1, 2]
        enonce_propre <- str_replace(enonce, fixed(m[1, 1]), "") |> str_trim()
        return(list(enonce = enonce_propre, indices = bloc))
    }

    m2 <- str_match(enonce, "(?s)\\[Indices?\\s*:\\s*(.*?)\\]\\s*$")
    if (!is.na(m2[1, 1])) {
        bloc <- m2[1, 2]
        enonce_propre <- str_replace(enonce, fixed(m2[1, 1]), "") |> str_trim()
        return(list(enonce = enonce_propre, indices = bloc))
    }

    list(enonce = enonce, indices = NULL)
}

# Extraire l'astuce (\emph{Astuce}: ...) de l'énoncé
extraire_astuce <- function(enonce) {
    m <- str_match(enonce, "\\(\\\\emph\\{Astuce\\}\\s*:\\s*([^)]+)\\)")
    if (!is.na(m[1, 1])) {
        astuce <- str_trim(m[1, 2])
        enonce_propre <- str_replace(enonce, fixed(m[1, 1]), "") |> str_trim()
        return(list(enonce = enonce_propre, astuce = astuce))
    }
    list(enonce = enonce, astuce = NULL)
}

# ── 5. Construire le contenu d'un fichier exercice ─────────────────────────────

construire_fichier_exercice <- function(enonce, rep, sol) {
    parties <- character()

    # Énoncé
    parties <- c(parties, enonce, "")

    # Callout Réponse
    if (!is.null(rep) && nchar(str_trim(rep)) > 0) {
        parties <- c(
            parties,
            '::: {.callout-note collapse="true" title="Éléments de réponse"}',
            rep,
            ":::",
            ""
        )
    }

    # Callout Solution
    if (!is.null(sol) && nchar(str_trim(sol)) > 0) {
        parties <- c(
            parties,
            '::: {.callout-tip collapse="true" title="Solution complète"}',
            sol,
            ":::",
            ""
        )
    }

    paste(parties, collapse = "\n")
}

# ── 6. Conversion d'un fichier .Rnw complet ────────────────────────────────────

convertir_rnw <- function(chemin_rnw, dossier_sortie, label_map_global) {
    message("\n── ", path_file(chemin_rnw))

    lignes <- readLines(chemin_rnw, encoding = "UTF-8", warn = FALSE)
    brut <- paste(lignes, collapse = "\n")

    # Titre du chapitre
    titre_chap <- str_match(brut, "\\\\chapter\\{([^}]+)\\}")[, 2]
    titre_chap <- if (is.na(titre_chap)) path_ext_remove(path_file(chemin_rnw)) else titre_chap

    # Slug du chapitre pour le sous-dossier
    slug_chap <- path_ext_remove(path_file(chemin_rnw)) |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9]", "_")

    # ── Passe 1 : collecter tous les labels du fichier ──────────────────────────
    blocs_bruts <- str_extract_all(
        brut,
        "(?s)\\\\begin\\{exercice\\}.*?\\\\end\\{exercice\\}"
    )[[1]]

    if (length(blocs_bruts) == 0) {
        message("  aucun exercice trouvé.")
        return(list(titre = titre_chap, slug = slug_chap, includes = character()))
    }

    # Générer les slugs provisoires depuis \filename ou le texte
    slugs_prov <- character(length(blocs_bruts))
    for (i in seq_along(blocs_bruts)) {
        inner <- str_match(
            blocs_bruts[i],
            "(?s)\\\\begin\\{exercice\\}(.*?)\\\\end\\{exercice\\}"
        )[, 2]
        meta <- extraire_meta_exercice(inner)
        if (!is.null(meta$filename)) {
            slugs_prov[i] <- str_replace(meta$filename, "^ex:", "")
        } else {
            # Fallback : générer un slug depuis le texte de l'énoncé
            rep_m <- str_match(inner, "(?s)\\\\begin\\{rep\\}(.*?)\\\\end\\{rep\\}")
            sol_m <- str_match(inner, "(?s)\\\\begin\\{sol\\}(.*?)\\\\end\\{sol\\}")
            enonce_brut <- inner
            if (!is.na(rep_m[1, 1])) enonce_brut <- str_replace(enonce_brut, fixed(rep_m[1, 1]), "")
            if (!is.na(sol_m[1, 1])) enonce_brut <- str_replace(enonce_brut, fixed(sol_m[1, 1]), "")
            slugs_prov[i] <- make_slug(enonce_brut)
        }
    }
    slugs_prov <- slugs_uniques(slugs_prov)

    # Enregistrer les labels dans la table globale
    for (i in seq_along(blocs_bruts)) {
        id_quarto <- paste0("sec-", slug_chap, "-", slugs_prov[i])
        labs <- extraire_labels(blocs_bruts[i])
        for (lab in labs) {
            label_map_global[[lab]] <<- id_quarto
            message("  label: ", lab, " → ", id_quarto)
        }
    }

    # ── Passe 2 : générer les fichiers ──────────────────────────────────────────
    dir_ex <- path(dossier_sortie, slug_chap)
    dir_create(dir_ex)

    includes <- character(length(blocs_bruts))

    for (i in seq_along(blocs_bruts)) {
        inner <- str_match(
            blocs_bruts[i],
            "(?s)\\\\begin\\{exercice\\}(.*?)\\\\end\\{exercice\\}"
        )[, 2]

        # Extraire \filename et \title, puis nettoyer l'inner
        meta_ex <- extraire_meta_exercice(inner)
        inner <- meta_ex$inner # énoncé sans \filename et \title
        titre_ex <- if (!is.null(meta_ex$titre)) {
            meta_ex$titre
        } else {
            str_to_sentence(str_replace_all(slugs_prov[i], "_", " "))
        }

        # Séparer les blocs réponse et solution
        rep_m <- str_match(inner, "(?s)\\\\begin\\{rep\\}(.*?)\\\\end\\{rep\\}")
        sol_m <- str_match(inner, "(?s)\\\\begin\\{sol\\}(.*?)\\\\end\\{sol\\}")

        enonce_brut <- inner
        if (!is.na(rep_m[1, 1])) enonce_brut <- str_replace(enonce_brut, fixed(rep_m[1, 1]), "")
        if (!is.na(sol_m[1, 1])) enonce_brut <- str_replace(enonce_brut, fixed(sol_m[1, 1]), "")

        rep_brut <- if (!is.na(rep_m[1, 1])) rep_m[1, 2] else NULL
        sol_brut <- if (!is.na(sol_m[1, 1])) sol_m[1, 2] else NULL

        # Supprimer les \label{} (déjà enregistrés)
        enonce_brut <- supprimer_labels(enonce_brut)
        sol_brut <- if (!is.null(sol_brut)) supprimer_labels(sol_brut) else NULL

        # Extraire indices et astuce AVANT transformation (regex sur LaTeX brut)
        res_indices <- extraire_indices(enonce_brut)
        enonce_brut <- res_indices$enonce
        indices_brut <- res_indices$indices

        res_astuce <- extraire_astuce(enonce_brut)
        enonce_brut <- res_astuce$enonce
        astuce_brut <- res_astuce$astuce

        # Résoudre les références croisées (sur le brut LaTeX)
        enonce_brut <- remplacer_refs(enonce_brut, label_map_global)
        sol_brut <- if (!is.null(sol_brut)) remplacer_refs(sol_brut, label_map_global) else NULL

        # Transformer vers Markdown
        enonce_md <- transformer_bloc(enonce_brut)
        rep_md <- if (!is.null(rep_brut)) transformer_bloc(rep_brut) else NULL
        sol_md <- if (!is.null(sol_brut)) transformer_bloc(sol_brut) else NULL

        # Callouts spéciaux (indices, astuce) — transformés séparément
        callouts_extra <- ""

        if (!is.null(indices_brut)) {
            indices_md <- transformer_bloc(indices_brut)
            callouts_extra <- paste0(
                callouts_extra,
                '::: {.callout-note collapse="true" title="Indices"}\n',
                indices_md, "\n:::\n\n"
            )
        }

        if (!is.null(astuce_brut)) {
            astuce_md <- latex_vers_md(astuce_brut)
            callouts_extra <- paste0(
                callouts_extra,
                '::: {.callout-tip collapse="true" title="Astuce"}\n',
                astuce_md, "\n:::\n\n"
            )
        }

        # ID Quarto de cet exercice
        id_quarto <- paste0("sec-", slug_chap, "-", slugs_prov[i])

        # Contenu du fichier
        contenu <- paste0(
            "## ", titre_ex, " {#", id_quarto, "}\n\n",
            construire_fichier_exercice(
                paste0(enonce_md, if (nchar(callouts_extra) > 0) paste0("\n\n", callouts_extra) else ""),
                rep_md,
                sol_md
            )
        )

        nom_fichier <- paste0("ex_", slugs_prov[i], ".qmd")
        writeLines(contenu, path(dir_ex, nom_fichier))
        includes[i] <- nom_fichier
        message("  ✓ ", nom_fichier)
    }

    list(titre = titre_chap, slug = slug_chap, includes = includes)
}

# ── 7. Main ────────────────────────────────────────────────────────────────────

# Table de correspondance label LaTeX → id Quarto (partagée entre chapitres)
label_map_global <- list()

dir_create(DOSSIER_SORTIE)

fichiers_rnw <- dir_ls(DOSSIER_SOURCE, glob = "*.Rnw", recurse = FALSE)

if (length(fichiers_rnw) == 0) {
    stop("Aucun fichier .Rnw trouvé dans : ", DOSSIER_SOURCE)
}

resultats <- list()
chap_qmds <- character()

# Deux passes :
#   Passe A — collecter TOUS les labels de tous les chapitres
#   Passe B — générer les fichiers (les refs inter-chapitres seront résolues)

message("\n═══ Passe A : collecte des labels ═══")
for (f in fichiers_rnw) {
    lignes <- readLines(f, encoding = "UTF-8", warn = FALSE)
    brut <- paste(lignes, collapse = "\n")
    blocs <- str_extract_all(
        brut,
        "(?s)\\\\begin\\{exercice\\}.*?\\\\end\\{exercice\\}"
    )[[1]]
    slug_chap <- path_ext_remove(path_file(f)) |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9]", "_")

    slugs_tmp <- character(length(blocs))
    for (i in seq_along(blocs)) {
        inner <- str_match(
            blocs[i],
            "(?s)\\\\begin\\{exercice\\}(.*?)\\\\end\\{exercice\\}"
        )[, 2]
        meta <- extraire_meta_exercice(inner)
        if (!is.null(meta$filename)) {
            slugs_tmp[i] <- str_replace(meta$filename, "^ex:", "")
        } else {
            rep_m <- str_match(inner, "(?s)\\\\begin\\{rep\\}(.*?)\\\\end\\{rep\\}")
            sol_m <- str_match(inner, "(?s)\\\\begin\\{sol\\}(.*?)\\\\end\\{sol\\}")
            enonce_tmp <- inner
            if (!is.na(rep_m[1, 1])) enonce_tmp <- str_replace(enonce_tmp, fixed(rep_m[1, 1]), "")
            if (!is.na(sol_m[1, 1])) enonce_tmp <- str_replace(enonce_tmp, fixed(sol_m[1, 1]), "")
            slugs_tmp[i] <- make_slug(enonce_tmp)
        }
    }
    slugs_tmp <- slugs_uniques(slugs_tmp)

    for (i in seq_along(blocs)) {
        id_q <- paste0("sec-", slug_chap, "-", slugs_tmp[i])
        labs <- extraire_labels(blocs[i])
        for (lab in labs) {
            label_map_global[[lab]] <- id_q
        }
    }
}

# Labels hors exercices (ex: au niveau chapitre)
for (f in fichiers_rnw) {
    brut <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    slug_chap <- path_ext_remove(path_file(f)) |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9]", "_")
    chap_lab <- str_match(brut, "\\\\label\\{chap:([^}]+)\\}")[, 2]
    if (!is.na(chap_lab)) {
        label_map_global[[paste0("chap:", chap_lab)]] <- slug_chap
    }
}

message("  ", length(label_map_global), " labels collectés.")
saveRDS(label_map_global, "_label_map.rds")

message("\n═══ Passe B : génération des fichiers ═══")
for (f in fichiers_rnw) {
    info <- convertir_rnw(f, DOSSIER_SORTIE, label_map_global)
    resultats[[info$slug]] <- info

    # Fichier agrégateur du chapitre
    chap_qmd <- path(DOSSIER_SORTIE, paste0(info$slug, ".qmd"))
    lignes_include <- paste0(
        "{{< include ", info$slug, "/", info$includes, " >}}",
        collapse = "\n\n"
    )
    writeLines(
        paste0("# ", info$titre, "\n\n", lignes_include, "\n"),
        chap_qmd
    )
    chap_qmds <- c(chap_qmds, paste0(DOSSIER_SORTIE, "/", info$slug, ".qmd"))
    message("  → ", chap_qmd)
}

# ── 8. Générer _quarto.yml ─────────────────────────────────────────────────────

yml_chapitres <- paste0("        - ", chap_qmds, collapse = "\n")

yml <- paste0(
    'project:
  type: book

book:
  title: "ACT2000 \u2014 Recueil d\'exercices"
  author: "Marie-Pier C\u00f4t\u00e9"
  language: fr
  chapters:
    - index.qmd
    - part: "Exercices"
      chapters:
', yml_chapitres, '
  search: true
  sidebar:
    style: docked

format:
  html:
    theme: cosmo
    toc: true
    number-sections: true
    code-fold: true
    include-in-header:
      text: |
        <script>
        window.MathJax = {
          tex: {
            macros: {
              esp:    ["\\\\mathbb{E}\\\\!\\\\left[#1\\\\right]", 1],
              Esp:    ["\\\\mathbb{E}\\\\!\\\\left[#1\\\\right]", 1],
              ex:     "\\\\mathbb{E}",
              var:    ["\\\\mathrm{Var}\\\\!\\\\left(#1\\\\right)", 1],
              Var:    ["\\\\mathrm{Var}\\\\!\\\\left(#1\\\\right)", 1],
              vr:     "\\\\mathrm{Var}",
              prob:   "\\\\Pr",
              Prob:   "\\\\Pr",
              Cov:    "\\\\mathrm{Cov}",
              d:      "\\\\mathrm{d}",
              bm:     ["\\\\boldsymbol{#1}", 1]
            }
          }
        };
        </script>

lang: fr
'
)

writeLines(yml, "_quarto.yml")
message("\n✓ _quarto.yml généré")

# ── 9. Créer un index.qmd minimal si absent ───────────────────────────────────
if (!file_exists("index.qmd")) {
    writeLines(
        paste0(
            "# Bienvenue {.unnumbered}\n\n",
            "Ce recueil contient les exercices du cours ACT2000.\n"
        ),
        "index.qmd"
    )
    message("✓ index.qmd créé")
}

message("\n✅ Conversion terminée. Lancer : quarto preview")
