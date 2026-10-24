# Índices de modelos bifactor y de segundo orden (Rodriguez et al., 2016)

# Cargas estandarizadas en forma bifactor: general (vector) y específicas (matriz)
#' @noRd
cargas_bifactor <- function(fit, general) {
  s <- lavaan::standardizedSolution(fit)
  if ("group" %in% names(s)) s <- s[s$group == 1, ]
  lv <- lavaan::lavNames(fit, "lv"); L <- s[s$op == "=~", ]
  orden2 <- unique(L$lhs[L$rhs %in% lv])
  if (length(orden2)) {
    # Segundo orden: transformación de Schmid-Leiman
    g <- orden2[1]; prim <- L$rhs[L$lhs == g]
    gam <- stats::setNames(L$est.std[L$lhs == g], prim)
    L1 <- L[L$lhs %in% prim & !L$rhs %in% lv, ]
    items <- unique(L1$rhs)
    lg <- stats::setNames(numeric(length(items)), items); LS <- matrix(0, length(items), length(prim), dimnames = list(items, prim))
    for (i in seq_len(nrow(L1))) {
      lg[L1$rhs[i]] <- lg[L1$rhs[i]] + L1$est.std[i] * gam[[L1$lhs[i]]]
      LS[L1$rhs[i], L1$lhs[i]] <- L1$est.std[i] * sqrt(1 - gam[[L1$lhs[i]]]^2)
    }
    return(list(general = g, lg = lg, LS = LS, tipo = "segundo_orden"))
  }
  items <- unique(L$rhs)
  if (is.null(general)) {
    cnt <- table(L$lhs); general <- names(cnt)[which.max(cnt)]
    if (max(cnt) < 0.8 * length(items))
      stop("No se reconoce un factor general (que cargue en casi todos los \u00edtems). Ind\u00edcalo con `general = \"...\"`.", call. = FALSE)
  }
  esp <- setdiff(unique(L$lhs), general)
  lg <- stats::setNames(numeric(length(items)), items); LS <- matrix(0, length(items), length(esp), dimnames = list(items, esp))
  for (i in seq_len(nrow(L))) {
    if (L$lhs[i] == general) lg[L$rhs[i]] <- L$est.std[i] else LS[L$rhs[i], L$lhs[i]] <- L$est.std[i]
  }
  cor_lv <- lavaan::lavInspect(fit, "cor.lv"); if (is.list(cor_lv)) cor_lv <- cor_lv[[1]]
  if (any(abs(cor_lv[upper.tri(cor_lv)]) > 1e-6))
    warning("Los factores no son ortogonales; los \u00edndices bifactor suponen factores independientes (orthogonal = TRUE en lavaan).", call. = FALSE)
  list(general = general, lg = lg, LS = LS, tipo = "bifactor")
}

#' @noRd
indices_bifactor <- function(lg, LS) {
  th <- 1 - lg^2 - rowSums(LS^2)
  total <- sum(lg)^2 + sum(colSums(LS)^2) + sum(th)
  grupo <- apply(LS, 1, function(r) if (any(r != 0)) colnames(LS)[which.max(abs(r))] else NA)
  esp <- colnames(LS)
  omega_s <- vapply(esp, function(k) { it <- names(grupo)[grupo %in% k]
    tk <- sum(lg[it])^2 + sum(LS[it, k])^2 + sum(th[it])
    c((sum(lg[it])^2 + sum(LS[it, k])^2) / tk, sum(LS[it, k])^2 / tk) }, numeric(2))
  H <- function(l) { l <- l[l != 0]; if (any(abs(l) >= 1)) return(NA_real_); 1 / (1 + 1 / sum(l^2 / (1 - l^2))) }
  Lam <- cbind(lg, LS); Sig <- Lam %*% t(Lam) + diag(th)
  fd <- if (any(th <= 0)) rep(NA_real_, ncol(Lam)) else sqrt(diag(t(Lam) %*% solve(Sig) %*% Lam))
  fd[!is.na(fd) & fd > 1] <- NA
  n <- length(lg); nk <- table(factor(grupo, levels = esp))
  ecv_ss <- vapply(esp, function(k) { it <- names(grupo)[grupo %in% k]; sum(LS[it, k]^2) / (sum(lg[it]^2) + sum(LS[it, k]^2)) }, numeric(1))
  list(omegaT = (sum(lg)^2 + sum(colSums(LS)^2)) / total, omegaH = sum(lg)^2 / total,
       omegaS = omega_s[1, ], omegaHS = omega_s[2, ],
       ecv = sum(lg^2) / (sum(lg^2) + sum(LS^2)), ecv_ss = ecv_ss,
       puc = 1 - sum(nk * (nk - 1) / 2) / (n * (n - 1) / 2),
       H = if (any(th <= 0)) rep(NA_real_, 1 + ncol(LS)) else c(H(lg), apply(LS, 2, H)), fd = fd, n_items = c(n, as.numeric(nk)), heywood = names(th)[th <= 0])
}

#' Tabla de índices de un modelo bifactor (o de segundo orden)
#'
#' Calcula los índices que hoy se piden al validar una escala con un modelo
#' bifactor (Reise et al., 2013; Rodriguez et al., 2016):
#'
#' * **\eqn{\omega} y \eqn{\omega_H}:** confiabilidad del puntaje total y la
#'   parte de ella que se debe solo al factor general (omega jerárquico).
#' * **\eqn{\omega_S} y \eqn{\omega_{HS}}:** lo mismo para cada subescala; el
#'   \eqn{\omega_{HS}} es la confiabilidad que aporta el factor específico más
#'   allá del general.
#' * **ECV:** proporción de la varianza común explicada por el factor general
#'   (y, en cada subescala, por su factor específico).
#' * **PUC:** proporción de correlaciones no contaminadas por factores
#'   específicos.
#' * **H:** replicabilidad del factor (Hancock y Mueller, 2001).
#' * **FD:** determinación de los puntajes factoriales.
#'
#' Criterios habituales: con \eqn{\omega_H} \eqn{\ge} .80, ECV \eqn{\ge} .70 y
#' PUC \eqn{\ge} .70 la escala puede tratarse como esencialmente
#' unidimensional; H \eqn{\ge} .80 indica un factor bien definido y FD
#' \eqn{\ge} .90 puntajes factoriales confiables.
#'
#' También funciona con un **modelo de segundo orden**: se aplica la
#' transformación de Schmid-Leiman (el factor de segundo orden hace de
#' general y cada factor de primer orden aporta su parte única).
#'
#' Los índices se calculan con las cargas estandarizadas. El modelo bifactor
#' debe tener los factores ortogonales (`orthogonal = TRUE` en `lavaan`).
#'
#' @param fit Modelo bifactor o de segundo orden estimado con [lavaan::cfa()].
#' @param general Nombre del factor general (se detecta solo si carga en casi
#'   todos los ítems).
#' @inheritParams tabla_descriptivos
#' @return Un objeto `tabla_apa`; los índices están en `$datos`.
#' @examples
#' library(lavaan)
#' modelo <- "g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
#'            visual =~ x1 + x2 + x3
#'            textual =~ x4 + x5 + x6
#'            speed =~ x7 + x8 + x9"
#' fit <- cfa(modelo, data = HolzingerSwineford1939, orthogonal = TRUE, std.lv = TRUE)
#' tabla_bifactor(fit)
#' @export
tabla_bifactor <- function(fit, general = NULL, etiquetas = NULL, decimales = 2, titulo = NULL, archivo = NULL) {
  if (!inherits(fit, "lavaan")) stop("`fit` debe ser un modelo de lavaan.", call. = FALSE)
  B <- cargas_bifactor(fit, general)
  I <- indices_bifactor(B$lg, B$LS)
  fac <- c(B$general, colnames(B$LS)); nf <- renombrar(fac, etiquetas)
  guion <- function(x) ifelse(is.na(x), "\u2014", f_apa(x, decimales))
  cel <- cbind(nf, formatC(I$n_items, format = "d"),
               f_apa(c(I$omegaT, I$omegaS), decimales), f_apa(c(I$omegaH, I$omegaHS), decimales),
               f_apa(c(I$ecv, I$ecv_ss), decimales), guion(I$H), guion(I$fd))
  neg <- matrix(FALSE, nrow(cel), ncol(cel))
  neg[1, 4] <- I$omegaH >= .80; neg[1, 5] <- I$ecv >= .70
  neg[, 6] <- !is.na(I$H) & I$H >= .80; neg[, 7] <- !is.na(I$fd) & I$fd >= .90
  if (length(I$heywood))
    warning("Caso Heywood (varianza de error negativa) en: ", paste(I$heywood, collapse = ", "),
            ". Los \u00edndices no son confiables; revisa el modelo (por ejemplo, un factor espec\u00edfico con cargas muy bajas).", call. = FALSE)
  unid <- I$omegaH >= .80 && I$ecv >= .70 && I$puc >= .70
  bajos <- nf[-1][I$omegaHS < .50]
  nota <- paste0(
    if (B$tipo == "segundo_orden") tx("\u00cdndices obtenidos con la transformaci\u00f3n de Schmid-Leiman del modelo de segundo orden. ",
                                      "Indices obtained from the Schmid-Leiman transformation of the second-order model. ") else "",
    tx("En la fila del factor general: \u03c9 = omega total, \u03c9H = omega jer\u00e1rquico, ECV = varianza com\u00fan explicada por el factor general. En las subescalas: \u03c9S, \u03c9HS y ECV de la subescala. H = replicabilidad del factor; FD = determinaci\u00f3n factorial. PUC = ",
       "General factor row: \u03c9 = omega total, \u03c9H = omega hierarchical, ECV = explained common variance of the general factor. Subscale rows: \u03c9S, \u03c9HS, and subscale ECV. H = construct replicability; FD = factor determinacy. PUC = "),
    f_apa(I$puc, 2), ". ",
    tx("Criterios: \u03c9H \u2265 .80, ECV \u2265 .70 y PUC \u2265 .70 indican una escala esencialmente unidimensional (Rodriguez et al., 2016); H \u2265 .80 (Hancock y Mueller, 2001) y FD \u2265 .90 (Gorsuch, 1983). En negritas, los valores que cumplen. ",
       "Criteria: \u03c9H \u2265 .80, ECV \u2265 .70, and PUC \u2265 .70 indicate an essentially unidimensional scale (Rodriguez et al., 2016); H \u2265 .80 (Hancock & Mueller, 2001) and FD \u2265 .90 (Gorsuch, 1983). Values meeting the criteria are in bold. "),
    if (unid) tx("La escala puede tratarse como esencialmente unidimensional.", "The scale can be treated as essentially unidimensional.")
    else tx("La escala no es esencialmente unidimensional seg\u00fan estos criterios.", "The scale is not essentially unidimensional by these criteria."),
    if (length(I$heywood)) paste0(tx(" Atenci\u00f3n: caso Heywood (varianza de error negativa) en ", " Caution: Heywood case (negative error variance) for "),
                                  paste(I$heywood, collapse = ", "), tx("; los \u00edndices no son confiables y H y FD no se calculan (\u2014).", "; the indices are not trustworthy and H and FD are not computed (\u2014).")) else "",
    if (length(bajos)) paste0(tx(" Subescalas con \u03c9HS < .50 (aportan poca confiabilidad propia m\u00e1s all\u00e1 del factor general): ",
                                 " Subscales with \u03c9HS < .50 (little reliable variance beyond the general factor): "), paste(bajos, collapse = ", "), ".") else "")
  t <- nueva_tabla(if (is.null(titulo)) tx("\u00cdndices del modelo bifactor", "Bifactor model indices") else titulo,
                   c("Factor", tx("\u00cdtems", "Items"), "\u03c9 / \u03c9S", "\u03c9H / \u03c9HS", "ECV", "H", "FD"), cel, neg,
                   nota = nota, separadores = 1,
                   datos = c(I, list(general = B$general, cargas_general = B$lg, cargas_especificas = B$LS, tipo = B$tipo)))
  if (!is.null(archivo)) guardar_tabla(t, archivo)
  t
}
