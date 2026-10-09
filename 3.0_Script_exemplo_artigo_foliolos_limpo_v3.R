precisa <- c("openxlsx", "ggplot2", "patchwork", "caret", "ranger", "glmnet", "mgcv", "FNN", "e1071", "gbm", "arm", "nnet")
faltam <- precisa[!sapply(precisa, requireNamespace, quietly = TRUE)]
if (length(faltam)) stop("Instale: install.packages(c(\"", paste(faltam, collapse = "\", \""), "\"))")
suppressPackageStartupMessages(library(ggplot2))
tem_xgb <- requireNamespace("xgboost", quietly = TRUE)

CFG <- list(
  semente = 123, arquivo = "dados_artigo4.xlsx", saidas = "saidas_artigo4",
  rapido = FALSE,
  n_folds = 20, n_rep = 10, prop_teste = 0.20,
  n_parcelas_I = 464, fotos_por_parcela = 30, n_cultivares_II = 15, n_linhagens_III = 215, reps = 2, foliolos = 3,
  iqr_k = 1.5, exgr = "diferenca",
  n_relief = 4000, k_relief = 30,
  metodo_final = "Gradient Boosting",
  indices_modelo = c("VARI", "ExGR"),
  n_indices = 2, colin_r_max = 0.95,
  gb = list(arvores = 100, taxa = 0.10, profundidade = 1, subamostra = 1.0, min_obs_no = 3))
if (CFG$rapido) { CFG$n_folds <- 5; CFG$n_rep <- 3 }
dir.create(CFG$saidas, showWarnings = FALSE)
knitando <- isTRUE(getOption("knitr.in.progress"))

options(width = 170)
quiet <- function(expr) { suppressWarnings(suppressMessages({ capture.output(r <- expr); r })) }
n_tab <- 0; n_fig <- 0
mostra <- function(x, legenda = NULL, d = 2) {
  x[] <- lapply(x, function(v) if (is.numeric(v)) round(v, d) else v)
  if (knitando) { n_tab <<- n_tab + 1; return(knitr::kable(x, row.names = FALSE, caption = paste0("**Tabela ", n_tab, ".** ", legenda))) }
  if (!is.null(legenda)) cat("\n", strsplit(legenda, "\\. ")[[1]][1], "\n", sep = "")
  print(x, row.names = FALSE); invisible(x)
}
fig_md <- function(nome, legenda) {
  if (!knitando) return(invisible(NULL))
  n_fig <<- n_fig + 1
  knitr::asis_output(sprintf("\n\n![**Figura %d.** %s](%s){width=100%%}\n\n", n_fig, legenda, file.path(CFG$saidas, nome)))
}
mostra_fig <- function(g, nome, legenda, w = 9, h = 4.5) { ggsave(file.path(CFG$saidas, nome), g, width = w, height = h, dpi = 200, bg = "white"); fig_md(nome, legenda) }
salva <- function(x, nome) write.csv(x, file.path(CFG$saidas, paste0(nome, ".csv")), row.names = FALSE)
texto <- function(...) { s <- paste0(...); if (knitando) knitr::asis_output(paste0("\n\n", s, "\n\n")) else { cat("\n", gsub("\\*\\*|`", "", s), "\n", sep = ""); invisible(NULL) } }

rmse <- function(o, p) sqrt(mean((o - p)^2))
mae  <- function(o, p) mean(abs(o - p))
mape <- function(o, p) 100 * mean(abs(o - p) / o)
r2   <- function(o, p) 1 - sum((o - p)^2) / sum((o - mean(o))^2)
metricas <- function(o, p) data.frame(R2 = r2(o, p), RMSE = rmse(o, p), MAE = mae(o, p), MAPE = mape(o, p))

Breed_RGB <- function(dados, R, G, B) {
  R <- substitute(R)
  G <- substitute(G)
  B <- substitute(B)
  dados$BG <- dados[[B]] - dados[[G]]
  dados$BGI <- dados[[B]] / dados[[G]]
  dados$BI <- sqrt((dados[[R]]^2 + dados[[G]]^2 + dados[[B]]^2) / 3)
  dados$BIM <- sqrt((dados[[R]]*2 + dados[[G]]*2 + dados[[B]]*2) / 3)
  dados$BR <- dados[[B]] - dados[[R]]
  dados$bx <- 0.55 * ((dados[[B]] - (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]])) / (1.0 - 0.0722))
  dados$bxa <- (0.55 * ((dados[[B]] - (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]])) / (1.0 - 0.0722))) -
    (0.55 * ((dados[[R]] - (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]])) / (1.0 - 0.2126)))
  dados$CI <- (dados[[R]] - dados[[B]]) / dados[[R]]
  dados$CIVE <- (0.44 * dados[[R]] - 0.81 * dados[[G]] + 0.39 * dados[[B]]) / (dados[[R]] + dados[[G]] + dados[[B]]) + 18.79
  dados$ExB <- (1.4 * dados[[B]] - dados[[G]]) / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$EXG <- 2 * (dados[[G]] * (dados[[R]] + dados[[G]] + dados[[B]]) - (dados[[R]] / (dados[[R]] + dados[[G]] + dados[[B]])) - dados[[B]] / (dados[[R]] + dados[[G]] + dados[[B]]))
  dados$EXGRaw <- 2 * dados[[G]] - dados[[R]] - dados[[B]]
  dados$ExGR <- (2 * dados[[G]] - dados[[R]] - dados[[B]]) / (1.4 * dados[[R]] - dados[[G]])
  dados$ExR <- 1.4 * dados[[R]] - dados[[G]]
  dados$EXR <- (1.4 * dados[[R]] - dados[[G]]) / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$ExRM <- (2 * dados[[R]] - dados[[G]] - dados[[B]]) / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$GB <- dados[[G]] - dados[[B]]
  dados$GLAI <- (25 * (dados[[G]] - dados[[R]]) / (dados[[G]] + dados[[R]] - dados[[B]])) + 1.25
  dados$GLI <- ((dados[[G]] - dados[[R]]) + (dados[[G]] - dados[[B]])) / (dados[[G]] + dados[[R]] + dados[[G]] + dados[[B]])
  dados$GLI2 <- (2 * dados[[G]] - dados[[R]] + dados[[B]]) / (2 * dados[[G]] + dados[[R]] + dados[[B]])
  dados$GR <- dados[[G]] - dados[[R]]
  dados$GRAY <- 0.299 * dados[[R]] + 0.587 * dados[[G]] + 0.114 * dados[[B]]
  dados$GRAY2 <- ((dados[[R]]^2.2 + (1.5 * dados[[G]])^2.2 + (0.6 * dados[[B]])^2.2) / (1 + 1.5^2.2 + 0.6^2.2))^(1 / 2.2)
  dados$HI <- (2 * dados[[R]] - dados[[G]] - dados[[B]]) / (dados[[G]] - dados[[B]])
  dados$HUE <- atan(2 * (dados[[B]] - dados[[G]] - dados[[R]]) / (30.5 * (dados[[G]] - dados[[R]])))
  dados$I <- dados[[R]] + dados[[G]] + dados[[B]]
  dados$IGB <- dados[[G]] / dados[[B]]
  dados$IGR <- dados[[G]] / dados[[R]]
  dados$IRB <- dados[[R]] / dados[[B]]
  dados$L <- (dados[[R]] + dados[[G]] + dados[[B]]) / 3
  dados$Lx <- 0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]]
  dados$Lxa <- (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]]) - (0.55 * ((dados[[R]] - (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]])) / (1.0 - 0.2126)))
  dados$Lxb <- (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]]) - (0.55 * ((dados[[B]] - (0.2126 * dados[[R]] + 0.7152 * dados[[G]] + 0.0722 * dados[[B]])) / (1.0 - 0.0722)))
  dados$MGRVI <- (dados[[G]]^2 - dados[[R]]^2) / (dados[[G]]^2 + dados[[R]]^2)
  dados$NB <- dados[[B]] / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$NGBDI <- (dados[[G]] - dados[[B]]) / (dados[[G]] + dados[[B]])
  dados$NRBDI <- (dados[[R]] - dados[[B]]) / (dados[[R]] + dados[[B]])
  dados$NG <- dados[[G]] / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$NGRDI <- (dados[[G]] - dados[[R]]) / (dados[[G]] + dados[[R]])
  dados$NR <- dados[[R]] / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$RB <- dados[[R]] - dados[[B]]
  dados$RGBindex <- (dados[[G]]^2 - dados[[B]] * dados[[R]]) / (dados[[G]]^2 + dados[[B]] * dados[[R]])
  dados$RG <- dados[[R]] - dados[[G]]
  dados$RGRI <- dados[[R]] / dados[[G]]
  dados$S <- ((dados[[R]] + dados[[G]] + dados[[B]]) - 3 * dados[[B]]) / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$SCI <- (dados[[R]] - dados[[G]]) / (dados[[R]] + dados[[G]])
  dados$TGI <- (-0.5) * ((190) * (dados[[R]] - dados[[G]]) - (120) * (dados[[R]] - dados[[B]])) / (dados[[R]] + dados[[G]] + dados[[B]])
  dados$VARI <- (dados[[G]] - dados[[R]]) / (dados[[G]] + dados[[R]] - dados[[B]])
  dados$VEG <- dados[[G]] / (dados[[G]] + dados[[R]] + dados[[B]]) / ((dados[[R]] / (dados[[R]] + dados[[G]] + dados[[B]]))^0.667) * ((dados[[B]] / (dados[[R]] + dados[[G]] + dados[[B]]))^(1 - 0.667))
  return(dados)
}

simular_artigo4 <- function(cfg) {
  set.seed(cfg$semente)
  bandas <- function(v, s, ilum) {
    n <- length(v)
    R <- ilum * (98 - 16 * v + 12 * s) + rnorm(n, 0, 3); G <- ilum * (128 + 14 * v - 10 * s) + rnorm(n, 0, 3); B <- ilum * (62 + 2 * v + 3 * s) + rnorm(n, 0, 2.5)
    art <- runif(n) < 0.015; f <- ifelse(runif(n) < 0.5, 0.55, 1.5)
    R[art] <- R[art] * f[art]; G[art] <- G[art] * f[art]; B[art] <- B[art] * f[art]
    data.frame(R = pmin(pmax(R, 1), 255), G = pmin(pmax(G, 1), 255), B = pmin(pmax(B, 1), 255))
  }
  rend <- function(v, s, micro, g, off = 0, sd_e) 3500 + off + 260 * v - 130 * (v^2 - 1) - 150 * s * plogis(1.5 * v) + 150 * micro + g + rnorm(length(v), 0, sd_e)
  np <- cfg$n_parcelas_I
  micro <- as.numeric(scale(0.7 * sin(seq_len(np) / 25) + rnorm(np, 0, 0.7)))
  v <- 0.55 * micro + sqrt(1 - 0.55^2) * rnorm(np); s <- rnorm(np)
  yI <- rend(v, s, micro, 0, 0, 650)
  fI <- cfg$fotos_por_parcela; ip <- rep(seq_len(np), each = fI)
  I <- data.frame(conjunto = "I", unidade = sprintf("P%03d", ip), repeticao = NA_integer_, foliolo = rep(seq_len(fI), np),
                  bandas(v[ip] + rnorm(np * fI, 0, 0.35), s[ip] + rnorm(np * fI, 0, 0.35), exp(rnorm(np * fI, 0, 0.12))), RGHA = yI[ip])
  mk <- function(conj, nc, pref, sd_gv, sd_g, off, dv, ilum_m, sd_e) {
    gv <- rnorm(nc, 0, sd_gv); g <- sd_g * (0.4 * gv / sd_gv + sqrt(1 - 0.16) * rnorm(nc))
    gr <- expand.grid(cult = seq_len(nc), rep = seq_len(cfg$reps), fol = seq_len(cfg$foliolos))
    pl <- unique(gr[c("cult", "rep")]); n <- nrow(pl)
    vp <- gv[pl$cult] + dv + rnorm(n, 0, 0.5); sp <- rnorm(n); mp <- rnorm(n, 0, 0.5)
    yp <- rend(vp, sp, mp, g[pl$cult], off, sd_e)
    ix <- match(paste(gr$cult, gr$rep), paste(pl$cult, pl$rep)); m <- nrow(gr)
    data.frame(conjunto = conj, unidade = sprintf("%s%03d", pref, gr$cult), repeticao = gr$rep, foliolo = gr$fol,
               bandas(vp[ix] + rnorm(m, 0, 0.35), sp[ix] + rnorm(m, 0, 0.35), ilum_m * exp(rnorm(m, 0, 0.12))), RGHA = yp[ix])
  }
  II  <- mk("II", cfg$n_cultivares_II, "C", 0.7, 300, -300, -0.15, 1.03, 450)
  III <- mk("III", cfg$n_linhagens_III, "L", 0.9, 330, 0, 0, 1.00, 450)
  d <- rbind(I, II, III); d$RGHA <- round(d$RGHA, 1); d[c("R", "G", "B")] <- lapply(d[c("R", "G", "B")], round, 1); d
}

if (!file.exists(CFG$arquivo)) {
  sim <- simular_artigo4(CFG)
  dic <- data.frame(coluna = names(sim), descricao = c("conjunto: I (construção), II (testemunhas em outro ambiente) ou III (linhagens F5)",
    "parcela (conjunto I), cultivar (II) ou linhagem (III)", "repetição (II e III)", "folíolo fotografado", "banda vermelha média do folíolo (0-255)",
    "banda verde média (0-255)", "banda azul média (0-255)", "rendimento de grãos da parcela (kg/ha); igual para as fotos da mesma unidade e repetição"))
  openxlsx::write.xlsx(list(dados = sim, dicionario = dic), CFG$arquivo, overwrite = TRUE)
}
bruto <- openxlsx::read.xlsx(CFG$arquivo, sheet = "dados")
bruto[c("R", "G", "B", "RGHA")] <- lapply(bruto[c("R", "G", "B", "RGHA")], as.numeric)
bruto$parcela <- ifelse(bruto$conjunto == "I", bruto$unidade, paste(bruto$unidade, bruto$repeticao, sep = "_r"))
res <- do.call(rbind, lapply(split(bruto, bruto$conjunto), function(x) data.frame(conjunto = x$conjunto[1], fotos = nrow(x), unidades = length(unique(x$unidade)),
  parcelas = length(unique(x$parcela)), RGHA_medio = mean(x$RGHA), RGHA_dp = sd(x$RGHA), RGHA_min = min(x$RGHA), RGHA_max = max(x$RGHA))))
mostra(res, "Conjuntos de dados. RGHA em kg ha⁻¹; parcelas = combinações de unidade e repetição que compartilham o mesmo RGHA.", d = 0)

out_iqr <- function(x, k = CFG$iqr_k) { q <- quantile(x, c(.25, .75)); i <- diff(q); x < q[1] - k * i | x > q[2] + k * i }
bruto$out <- ave(seq_len(nrow(bruto)), bruto$conjunto, FUN = function(i) as.integer(out_iqr(bruto$R[i]) | out_iqr(bruto$G[i]) | out_iqr(bruto$B[i])))
tab_out <- do.call(rbind, lapply(split(bruto, bruto$conjunto), function(x) data.frame(conjunto = x$conjunto[1], antes = nrow(x), removidas = sum(x$out), depois = sum(x$out == 0), perc_removido = 100 * mean(x$out))))
mostra(tab_out, "Outliers removidos pelo IQR 1,5 nas bandas R, G e B, por conjunto.", d = 1)
dados <- bruto[bruto$out == 0, ]; dados$out <- NULL

dados <- as.data.frame(Breed_RGB(dados, R, G, B))
if (CFG$exgr == "diferenca") dados$ExGR <- dados$EXGRaw - dados$ExR
nomes_indices <- setdiff(names(dados), c("conjunto", "unidade", "repeticao", "foliolo", "R", "G", "B", "RGHA", "parcela"))
ruim <- nomes_indices[!sapply(nomes_indices, function(v) all(is.finite(dados[[v]])) && sd(dados[[v]]) > 0)]
indices <- setdiff(nomes_indices, ruim)
tab_idx <- data.frame(etapa = c("índices calculados pelo Breed_RGB", "descartados (não finitos ou constantes)", "índices candidatos"), n = c(length(nomes_indices), length(ruim), length(indices)),
                      detalhe = c("", paste(ruim, collapse = ", "), ""))
mostra(tab_idx, "Índices calculados e descartados antes da seleção.")

I <- dados[dados$conjunto == "I", ]
set.seed(CFG$semente)
i_tr <- as.vector(caret::createDataPartition(I$RGHA, p = 1 - CFG$prop_teste, list = FALSE)); I$grupo <- "validacao"; I$grupo[i_tr] <- "treino"
rrelief <- function(X, y, m = CFG$n_relief, k = CFG$k_relief) {
  X <- as.matrix(X); Xs <- apply(X, 2, function(x) rank(x) / length(x)); colnames(Xs) <- colnames(X); ys <- (y - min(y)) / (max(y) - min(y))
  q <- sample(nrow(Xs), min(m, nrow(Xs))); nn <- FNN::get.knnx(Xs, Xs[q, , drop = FALSE], k = k + 1)$nn.index[, -1, drop = FALSE]
  dy <- abs(ys[q] - matrix(ys[nn], nrow = length(q)))
  Pdc <- mean(dy); Pda <- Pdcda <- numeric(ncol(Xs))
  for (j in seq_len(ncol(Xs))) { da <- abs(Xs[q, j] - matrix(Xs[nn, j], nrow = length(q))); Pda[j] <- mean(da); Pdcda[j] <- mean(dy * da) }
  w <- Pdcda / Pdc - (Pda - Pdcda) / (1 - Pdc); names(w) <- colnames(Xs); w
}
w <- rrelief(I[I$grupo == "treino", indices], I$RGHA[I$grupo == "treino"])
rank_relief <- data.frame(posicao = seq_along(w), indice = names(sort(w, decreasing = TRUE)), ReliefF = sort(w, decreasing = TRUE), row.names = NULL)
rank_relief$r_RGHA <- sapply(rank_relief$indice, function(v) cor(I[[v]][I$grupo == "treino"], I$RGHA[I$grupo == "treino"]))
rank_relief$posicao_por_r <- rank(-abs(rank_relief$r_RGHA), ties.method = "min")
cm <- abs(cor(I[I$grupo == "treino", indices]))
esc <- c(); for (v in rank_relief$indice) if (length(esc) < CFG$n_indices && (!length(esc) || all(cm[v, esc] <= CFG$colin_r_max))) esc <- c(esc, v)
usar <- if (is.null(CFG$indices_modelo)) esc else CFG$indices_modelo
rr_tab <- rbind(head(rank_relief, 12), rank_relief[rank_relief$indice %in% setdiff(CFG$indices_modelo, head(rank_relief$indice, 12)), ]); rr_tab$ReliefF <- rr_tab$ReliefF * 1000; names(rr_tab)[3] <- "ReliefF_x1000"
mostra(rr_tab,
       "Regressão ReliefF: 12 melhores índices e a posição dos índices do Artigo 4 (VARI e ExGR) quando fora dessa lista. Pontuação (×1000) maior = índice que muda junto com o RGHA; r_RGHA = correlação simples e posicao_por_r = posição pelo |r|.", d = 3)
salva(rank_relief, "relief_ranking")
texto("**Cuidado ao ler o ReliefF:** ele procura vizinhos no espaço de todos os índices. Como cerca de 20 dos 49 índices medem brilho (BI, L, I, GRAY...), eles dominam a distância e podem subir no ranking sem relação com o RGHA; por isso o ranking é lido junto com a correlação simples. ",
      "**Índices que entram no modelo:** ", paste(usar, collapse = " e "), ". Escolha do ReliefF após filtro de colinearidade: ", paste(esc, collapse = " e "),
      ". Para o ReliefF decidir sozinho, use `CFG$indices_modelo <- NULL`.")

cc <- cor(I[I$grupo == "treino", usar]); vif <- diag(solve(cc))
mostra(data.frame(indice = usar, r_com_o_outro = if (length(usar) == 2) rep(cc[1, 2], 2) else NA, VIF = vif, r_com_RGHA = sapply(usar, function(v) cor(I[[v]][I$grupo == "treino"], I$RGHA[I$grupo == "treino"]))),
       "Índices do modelo final: correlação entre eles, VIF e correlação com o RGHA (treino do conjunto I).", d = 3)

info <- data.frame(Metodo = c("Lm", "Ridge", "Bayesian Ridge", "Lasso", "nLnn*", "GAM", "Random Forest", "XGBoost", "SVM", "KNN", "RNA", "BRM", "Deep Learning", "Gradient Boosting"),
  Formulacao = c("ŷ = β0 + Σ βj·xj, mínimos quadrados.",
    "Mínimos quadrados + λ·Σβj² (L2): encolhe coeficientes; estável com índices correlacionados.",
    "Regressão linear com prior normal nos coeficientes; precisão do erro e do prior estimadas pelos dados (maximização da evidência, como no BayesianRidge do scikit-learn).",
    "Mínimos quadrados + λ·Σ|βj| (L1): zera coeficientes.",
    "Polinômio de grau 2 em cada índice mais a interação (substituto de nLnn).",
    "ŷ = β0 + Σ fj(xj), com fj splines penalizadas: curvas suaves, sem interação.",
    "Média de 500 árvores em amostras bootstrap; capta limiares e interações.",
    "Gradient boosting com regularização e árvores de profundidade 4 (opcional; exige o pacote xgboost).",
    "Regressão por vetores de suporte com kernel radial; só pontos fora da margem ε definem a superfície.",
    "Média do RGHA dos k vizinhos mais próximos no espaço dos índices padronizados.",
    "Rede neural com 1 camada oculta (5 neurônios, logística) e saída linear.",
    "Regressão bayesiana (priors Cauchy fracos, pacote arm); a predição é a média posterior. Para brms use `brms::brm()`.",
    "Perceptron com 2 camadas ocultas (16 e 8 neurônios, ReLU), Adam, 400 épocas.",
    "Boosting com o ajuste do Artigo 4: 100 árvores, taxa 0,10, profundidade 1, subamostra 1,0 (scikit-learn; aqui pacote gbm)."),
  Ajuste = c("—", "λ por cv.glmnet (10 partes)", "automático", "λ por cv.glmnet (10 partes)", "—", "k = 5 por termo, REML", "mtry = 1 ou 2 (ranger)", "η = 0,1; 200 rodadas",
    "custo 1; γ = 1/p; ε = 0,1 (RGHA padronizado)", "k de {5, 10, 20, 40, 80, 160} por CV de 5 partes", "decay 0,01; 200 iterações", "padrão do arm", "lr 0,01; 400 épocas",
    "n.minobsinnode = 3 (aproxima o 'nós dividem subconjuntos menores que 3' do Artigo)"))
if (!tem_xgb) info <- info[info$Metodo != "XGBoost", ]
METODOS <- info$Metodo
if (knitando) mostra(info, paste0("Métodos comparados (", length(METODOS), " neste computador): formulação e ajuste. Todos usam as mesmas preditoras padronizadas; a resposta é o RGHA (kg ha⁻¹)."))

bayes_ridge <- function(X, y, iter = 200) {
  mx <- colMeans(X); yc <- y - mean(y); Xc <- sweep(X, 2, mx); n <- nrow(X); p <- ncol(X); XtX <- crossprod(Xc); ev <- eigen(XtX, symmetric = TRUE)$values
  a <- 1 / var(yc); l <- 1
  for (i in seq_len(iter)) {
    S <- solve(a * XtX + l * diag(p)); w <- a * S %*% crossprod(Xc, yc); g <- sum(a * ev / (l + a * ev))
    l2 <- g / sum(w^2); a2 <- (n - g) / sum((yc - Xc %*% w)^2); if (abs(l2 - l) + abs(a2 - a) < 1e-8 * (l + a)) break; l <- l2; a <- a2
  }
  function(Xn) as.numeric(mean(y) + sweep(Xn, 2, mx) %*% w)
}
mlp <- function(X, y, h = c(16, 8), epocas = 400, lr = 0.01, semente = 1) {
  set.seed(semente); ym <- mean(y); ys <- sd(y); yy <- (y - ym) / ys; tam <- c(ncol(X), h, 1); L <- length(tam) - 1
  W <- lapply(seq_len(L), function(l) matrix(rnorm(tam[l] * tam[l + 1], 0, sqrt(2 / tam[l])), tam[l])); b <- lapply(seq_len(L), function(l) rep(0, tam[l + 1]))
  mW <- lapply(W, function(w) w * 0); vW <- mW; mb <- lapply(b, function(x) x * 0); vb <- mb
  fwd <- function(X) { A <- list(X); for (l in seq_len(L)) { Z <- sweep(A[[l]] %*% W[[l]], 2, b[[l]], "+"); A[[l + 1]] <- if (l < L) pmax(Z, 0) else Z }; A }
  n <- nrow(X)
  for (e in seq_len(epocas)) {
    A <- fwd(X); d <- (A[[L + 1]] - yy) * 2 / n
    for (l in L:1) {
      gW <- crossprod(A[[l]], d); gb <- colSums(d); if (l > 1) d <- (d %*% t(W[[l]])) * (A[[l]] > 0)
      mW[[l]] <- 0.9 * mW[[l]] + 0.1 * gW; vW[[l]] <- 0.999 * vW[[l]] + 0.001 * gW^2; mb[[l]] <- 0.9 * mb[[l]] + 0.1 * gb; vb[[l]] <- 0.999 * vb[[l]] + 0.001 * gb^2
      W[[l]] <- W[[l]] - lr * (mW[[l]] / (1 - 0.9^e)) / (sqrt(vW[[l]] / (1 - 0.999^e)) + 1e-8); b[[l]] <- b[[l]] - lr * (mb[[l]] / (1 - 0.9^e)) / (sqrt(vb[[l]] / (1 - 0.999^e)) + 1e-8)
    }
  }
  function(Xn) as.numeric(fwd(Xn)[[L + 1]]) * ys + ym
}
ajusta <- function(tr, novos, vars, modelos = METODOS, resposta = "RGHA", semente = CFG$semente) {
  mu <- sapply(tr[vars], mean); dp <- sapply(tr[vars], sd)
  z <- function(d) { d <- as.data.frame(d[vars]); d[] <- Map(function(v, m, s) (v - m) / s, d, mu, dp); d }
  trz <- z(tr); y <- tr[[resposta]]; ym <- mean(y); ysd <- sd(y); yz <- (y - ym) / ysd
  Xtr <- as.matrix(trz); trz[[resposta]] <- y; f <- reformulate(vars, resposta); saida <- list(); falhas <- c()
  for (m in intersect(modelos, METODOS)) {
    set.seed(semente)
    pr <- tryCatch(switch(m,
      "Lm" = { a <- lm(f, trz); function(d) as.numeric(predict(a, d)) },
      "Ridge" = { a <- glmnet::glmnet(Xtr, y, alpha = 0, lambda = glmnet::cv.glmnet(Xtr, y, alpha = 0)$lambda.min); function(d) as.numeric(predict(a, as.matrix(d))) },
      "Bayesian Ridge" = { a <- bayes_ridge(Xtr, y); function(d) a(as.matrix(d)) },
      "Lasso" = { a <- glmnet::glmnet(Xtr, y, alpha = 1, lambda = glmnet::cv.glmnet(Xtr, y, alpha = 1)$lambda.min); function(d) as.numeric(predict(a, as.matrix(d))) },
      "nLnn*" = { ff <- reformulate(c(paste0("poly(", vars, ", 2)"), if (length(vars) == 2) paste(vars, collapse = ":")), resposta); a <- lm(ff, trz); function(d) as.numeric(predict(a, d)) },
      "GAM" = { a <- mgcv::gam(reformulate(paste0("s(", vars, ", k = 5)"), resposta), data = trz, method = "REML"); function(d) as.numeric(predict(a, d)) },
      "Random Forest" = { a <- ranger::ranger(f, trz, num.trees = 500, mtry = max(1, floor(sqrt(length(vars)))), seed = semente); function(d) predict(a, d)$predictions },
      "XGBoost" = { a <- xgboost::xgb.train(params = list(objective = "reg:squarederror", max_depth = 4, eta = 0.1, nthread = 1), data = xgboost::xgb.DMatrix(Xtr, label = y), nrounds = 200, verbose = 0)
        function(d) predict(a, xgboost::xgb.DMatrix(as.matrix(d))) },
      "SVM" = { a <- e1071::svm(Xtr, yz, type = "eps-regression", kernel = "radial", cost = 1, gamma = 1 / ncol(Xtr), epsilon = 0.1); function(d) as.numeric(predict(a, as.matrix(d))) * ysd + ym },
      "KNN" = { ks <- c(5, 10, 20, 40, 80, 160); fo <- caret::createFolds(y, k = 5)
        err <- sapply(ks, function(k) mean(sapply(fo, function(i) rmse(y[i], FNN::knn.reg(Xtr[-i, , drop = FALSE], Xtr[i, , drop = FALSE], y[-i], k = k)$pred))))
        k <- ks[which.min(err)]; function(d) FNN::knn.reg(Xtr, as.matrix(d), y, k = k)$pred },
      "RNA" = { a <- nnet::nnet(Xtr, yz, size = 5, decay = 0.01, linout = TRUE, maxit = 200, trace = FALSE); function(d) as.numeric(predict(a, as.matrix(d))) * ysd + ym },
      "BRM" = { a <- arm::bayesglm(f, data = trz, family = gaussian()); function(d) as.numeric(predict(a, d)) },
      "Deep Learning" = { a <- mlp(Xtr, y, semente = semente); function(d) a(as.matrix(d)) },
      "Gradient Boosting" = { g <- CFG$gb
        a <- gbm::gbm(f, data = trz, distribution = "gaussian", n.trees = g$arvores, shrinkage = g$taxa, interaction.depth = g$profundidade, bag.fraction = g$subamostra,
                      n.minobsinnode = g$min_obs_no, train.fraction = 1, verbose = FALSE, n.cores = 1)
        function(d) as.numeric(predict(a, d, n.trees = g$arvores)) }),
      error = function(e) { falhas <<- c(falhas, paste0(m, ": ", conditionMessage(e))); NULL })
    if (!is.null(pr)) saida[[m]] <- c(list(tr = pr(trz[vars])), lapply(novos, function(d) pr(z(d))))
  }
  attr(saida, "falhas") <- falhas; saida
}

TR <- I[I$grupo == "treino", ]; VA <- I[I$grupo == "validacao", ]
II_ <- dados[dados$conjunto == "II", ]; III_ <- dados[dados$conjunto == "III", ]
tempo <- system.time(ajuste <- ajusta(TR, list(val = VA, II = II_, III = III_), usar))
if (length(attr(ajuste, "falhas"))) stop("Falharam: ", paste(attr(ajuste, "falhas"), collapse = "; "))
tab_val <- do.call(rbind, lapply(names(ajuste), function(m) cbind(Metodo = m, metricas(VA$RGHA, ajuste[[m]]$val),
  R2_treino = r2(TR$RGHA, ajuste[[m]]$tr), RMSE_treino = rmse(TR$RGHA, ajuste[[m]]$tr))))
tab_val$gap_R2 <- tab_val$R2_treino - tab_val$R2
tab_val <- tab_val[order(tab_val$RMSE), ]
mostra(tab_val, paste0("Desempenho no conjunto de validação (20% do conjunto I; n = ", nrow(VA), " fotos), ordenado pelo RMSE. Colunas _treino: desempenho nas próprias fotos de treino (n = ", nrow(TR), "); gap_R2 = sobreajuste. RMSE e MAE em kg ha⁻¹; MAPE em %."), d = 3)
salva(tab_val, "validacao_20pct")
melhor <- tab_val$Metodo[1]

pl <- do.call(rbind, lapply(c(melhor, "Lm", "Random Forest"), function(m) data.frame(metodo = m, obs = VA$RGHA, pred = ajuste[[m]]$val)))
pl$metodo <- factor(pl$metodo, unique(c(melhor, "Lm", "Random Forest")))
mostra_fig(ggplot(pl, aes(pred, obs)) + geom_point(alpha = .08, size = .7, colour = "#2e7d32") + geom_abline(colour = "#b71c1c") + geom_smooth(method = "lm", se = FALSE, formula = y ~ x, colour = "black", linewidth = .6) +
  facet_wrap(~metodo) + labs(x = "RGHA predito (kg/ha)", y = "RGHA observado (kg/ha)") + theme_minimal(base_size = 11),
  "fig1_obs_pred.png", "RGHA observado × predito no conjunto de validação (20%). Linha vermelha: 1:1; preta: ajuste observado ~ predito. Predições encurtadas (reta mais plana que 1:1) são típicas de R² baixo.", 9, 3.6)

cv_metodos <- function(dd, folds, modelos = METODOS) {
  do.call(rbind, lapply(sort(unique(folds)), function(k) { a <- ajusta(dd[folds != k, ], list(te = dd[folds == k, ]), usar, modelos)
    do.call(rbind, lapply(names(a), function(m) cbind(parte = k, Metodo = m, metricas(dd$RGHA[folds == k], a[[m]]$te)))) }))
}
resume <- function(long) { r <- aggregate(cbind(R2, RMSE, MAE, MAPE) ~ Metodo, long, function(x) c(media = mean(x), dp = sd(x)))
  r <- do.call(data.frame, r); names(r) <- sub("\\.media", "", sub("\\.dp", "_dp", names(r))); r[order(r$RMSE), ] }
set.seed(CFG$semente); cv_long <- cv_metodos(TR, caret::createFolds(TR$RGHA, k = CFG$n_folds, list = FALSE))
tab_cv <- resume(cv_long)
mostra(tab_cv, paste0("Validação cruzada com ", CFG$n_folds, " partes estratificadas no treino: média das partes e desvio padrão (_dp), ordenado pelo RMSE."), d = 3); salva(tab_cv, "cv_partes")

rep_long <- do.call(rbind, lapply(seq_len(CFG$n_rep), function(r) { set.seed(CFG$semente + r); it <- as.vector(caret::createDataPartition(I$RGHA, p = 0.8, list = FALSE))
  a <- ajusta(I[it, ], list(te = I[-it, ]), usar); do.call(rbind, lapply(names(a), function(m) cbind(repeticao = r, Metodo = m, metricas(I$RGHA[-it], a[[m]]$te)))) }))
tab_rep <- resume(rep_long)
venceu <- table(factor(sapply(split(rep_long, rep_long$repeticao), function(x) x$Metodo[which.min(x$RMSE)]), tab_rep$Metodo)) / CFG$n_rep * 100
tab_rep$vitorias_RMSE_perc <- as.numeric(venceu[tab_rep$Metodo])
mostra(tab_rep, paste0(CFG$n_rep, " repetições de 80% treino e 20% teste: média, desvio padrão (_dp) e % de repetições em que o método teve o menor RMSE."), d = 3); salva(tab_rep, "repeticoes_80_20")
mostra_fig(ggplot(rep_long, aes(reorder(Metodo, RMSE, median), RMSE)) + geom_boxplot(fill = "#a5d6a7", outlier.size = .8) + coord_flip() +
  labs(x = NULL, y = "RMSE no teste (kg/ha)") + theme_minimal(base_size = 11),
  "fig2_rmse_repeticoes.png", paste0("RMSE nas ", CFG$n_rep, " repetições de 80/20, por método. Caixa pequena = resultado estável entre partições."), 8, 4.2)

top <- tab_rep[1, ]; sel <- tab_rep[tab_rep$Metodo == CFG$metodo_final, ]
tab_rep$empata_com_o_melhor <- tab_rep$RMSE <= top$RMSE + top$RMSE_dp
mostra(tab_rep[c("Metodo", "RMSE", "RMSE_dp", "R2", "empata_com_o_melhor")], paste0("Empate técnico: método com menor RMSE médio = ", top$Metodo, " (", round(top$RMSE, 1), " ± ", round(top$RMSE_dp, 1), " kg ha⁻¹). TRUE = RMSE médio até 1 desvio padrão acima do menor. Método final: ", CFG$metodo_final, "."), d = 2)

ext <- function(d, nome) do.call(rbind, lapply(names(ajuste), function(m) { ve <- mean(ajuste[[m]][[nome]] - d$RGHA); data.frame(Metodo = m, metricas(d$RGHA, ajuste[[m]][[nome]]), vies = ve, RMSE_sem_vies = sqrt(max(rmse(d$RGHA, ajuste[[m]][[nome]])^2 - ve^2, 0))) }))
base <- setNames(tab_val$RMSE, tab_val$Metodo)
e2 <- ext(II_, "II"); e3 <- ext(III_, "III")
for (x in c("e2", "e3")) { t <- get(x); t$RMSE_validacao_20 <- base[t$Metodo]; t$razao_RMSE <- t$RMSE / t$RMSE_validacao_20; assign(x, t[order(t$RMSE), ]) }
mostra(e2, paste0("Conjunto II (", length(unique(II_$unidade)), " cultivares, outro ambiente; n = ", nrow(II_), " fotos). vies = predito − observado (kg ha⁻¹); RMSE_sem_vies = RMSE se o viés médio fosse corrigido (recalibração do ambiente); razao_RMSE = RMSE externo / RMSE da validação 20% (≈ 1 = desempenho mantido)."), d = 3); salva(e2, "externo_II")
mostra(e3, paste0("Conjunto III (", length(unique(III_$unidade)), " linhagens F5, mesmo ambiente; n = ", nrow(III_), " fotos), mesmas colunas da Tabela anterior."), d = 3); salva(e3, "externo_III")

rank_unid <- function(d, nome) do.call(rbind, lapply(names(ajuste), function(m) {
  p <- tapply(ajuste[[m]][[nome]], d$unidade, mean); o <- tapply(d$RGHA, d$unidade, mean)[names(p)]; n20 <- ceiling(0.2 * length(p))
  data.frame(Metodo = m, r_unidade = cor(p, o), spearman = cor(p, o, method = "spearman"), top20_acertos = sum(names(sort(p, TRUE))[1:n20] %in% names(sort(o, TRUE))[1:n20]),
             top20_ao_acaso = n20^2 / length(p), n_selecionadas = n20) }))
r2_ <- rank_unid(II_, "II"); r3_ <- rank_unid(III_, "III")
mostra(r3_[order(-r3_$spearman), ], "Conjunto III, por linhagem (média das 6 fotos): correlação (r) e Spearman entre RGHA predito e observado e acertos na seleção do top 20% (top20_ao_acaso = acertos esperados sorteando).", d = 2); salva(r3_, "ranking_linhagens")
mostra(r2_[order(-r2_$spearman), ], "Conjunto II, por cultivar: mesmas colunas da tabela anterior (n pequeno: leia com cautela).", d = 2)

pe <- rbind(data.frame(conjunto = "II (outro ambiente)", obs = II_$RGHA, pred = ajuste[[sel$Metodo]]$II), data.frame(conjunto = "III (linhagens F5)", obs = III_$RGHA, pred = ajuste[[sel$Metodo]]$III))
mostra_fig(ggplot(pe, aes(pred, obs)) + geom_point(alpha = .3, size = .9, colour = "#1565c0") + geom_abline(colour = "#b71c1c") + geom_smooth(method = "lm", se = FALSE, formula = y ~ x, colour = "black", linewidth = .6) +
  facet_wrap(~conjunto) + labs(x = "RGHA predito (kg/ha)", y = "RGHA observado (kg/ha)", title = paste("Método selecionado:", sel$Metodo)) + theme_minimal(base_size = 11),
  "fig3_externos.png", "RGHA observado × predito nos conjuntos externos para o método selecionado. Linha vermelha: 1:1; preta: ajuste observado ~ predito.", 9, 3.8)

met_curva <- intersect(c("Lm", "GAM", "Random Forest", "KNN", "Deep Learning", "Gradient Boosting"), METODOS)
faixa <- function(v) cut(TR[[v]], unique(quantile(TR[[v]], seq(0, 1, .05))), include.lowest = TRUE)
cur <- do.call(rbind, lapply(usar, function(v) do.call(rbind, lapply(met_curva, function(m) data.frame(indice = v, metodo = m, x = as.numeric(tapply(TR[[v]], faixa(v), mean)), y = as.numeric(tapply(ajuste[[m]]$tr, faixa(v), mean)))))))
bin <- do.call(rbind, lapply(usar, function(v) data.frame(indice = v, x = as.numeric(tapply(TR[[v]], faixa(v), mean)), y = as.numeric(tapply(TR$RGHA, faixa(v), mean)))))
mostra_fig(ggplot() + geom_line(data = cur, aes(x, y, colour = metodo), linewidth = .8) + geom_point(data = bin, aes(x, y), colour = "grey20", size = 1.8) + facet_wrap(~indice, scales = "free_x") +
  labs(x = "valor do índice (média da faixa)", y = "RGHA (kg/ha)", colour = NULL) + theme_minimal(base_size = 11) + theme(legend.position = "bottom"),
  "fig4_curvas.png", "RGHA médio observado (pontos) e predito por método (linhas) em 20 faixas de cada índice, nas fotos de treino. Reta × curva mostra o que cada família consegue representar.", 9, 4.2)

diag2 <- merge(tab_val[c("Metodo", "R2", "R2_treino", "gap_R2")], tab_rep[c("Metodo", "RMSE", "RMSE_dp")], by = "Metodo", suffixes = c("", "_rep"))
names(diag2) <- c("Metodo", "R2_validacao", "R2_treino", "gap_R2", "RMSE_repeticoes", "RMSE_dp_repeticoes")
diag2 <- merge(diag2, e3[c("Metodo", "razao_RMSE")], by = "Metodo"); names(diag2)[7] <- "razao_RMSE_III"; diag2 <- diag2[order(diag2$RMSE_repeticoes), ]
mostra(diag2, "Sobreajuste (gap_R2 = R² treino − R² validação), estabilidade (desvio do RMSE nas repetições) e transferência para as linhagens (razão_RMSE_III = RMSE no conjunto III / RMSE da validação 20%).", d = 3)

set.seed(CFG$semente); f_lin <- caret::createFolds(I$RGHA, k = 5, list = FALSE)
pm <- tapply(I$RGHA, I$parcela, mean); f_par <- setNames(caret::createFolds(pm, k = 5, list = FALSE), names(pm))[I$parcela]
cvl <- resume(cv_metodos(I, f_lin)); cvp <- resume(cv_metodos(I, f_par))
ind <- merge(cvl[c("Metodo", "R2", "RMSE")], cvp[c("Metodo", "R2", "RMSE")], by = "Metodo", suffixes = c("_fotos", "_parcela"))
ind$aumento_RMSE_perc <- 100 * (ind$RMSE_parcela / ind$RMSE_fotos - 1); ind <- ind[order(ind$RMSE_parcela), ]
mostra(ind, "Validação cruzada de 5 partes no conjunto I, sorteando fotos (_fotos) ou parcelas inteiras (_parcela). aumento_RMSE_perc = piora ao deixar parcelas inteiras de fora.", d = 3); salva(ind, "independencia")

fm <- function(x, d = 1) format(round(x, d), decimal.mark = ",", nsmall = d)
lm_ <- tab_val[tab_val$Metodo == "Lm", ]; gam_ <- tab_val[tab_val$Metodo == "GAM", ]; gb_ <- tab_val[tab_val$Metodo == "Gradient Boosting", ]
mx <- diag2[which.max(diag2$gap_R2), ]; est <- diag2[which.min(diag2$RMSE_dp_repeticoes), ]; pior <- ind[which.max(ind$aumento_RMSE_perc), ]
texto("- **Desempenho:** menor RMSE médio em ", CFG$n_rep, " repetições: ", top$Metodo, " (", fm(top$RMSE), " ± ", fm(top$RMSE_dp), " kg/ha). ", sum(tab_rep$empata_com_o_melhor), " de ", nrow(tab_rep),
      " métodos empatam com ele (até 1 desvio padrão). Método final (", CFG$metodo_final, "): RMSE ", fm(sel$RMSE), " kg/ha, R² ", fm(sel$R2, 2), ", MAPE ", fm(sel$MAPE), "%.\n",
      "- **Curvatura:** na validação, R² de Lm = ", fm(lm_$R2, 3), ", GAM = ", fm(gam_$R2, 3), " e Gradient Boosting = ", fm(gb_$R2, 3), ". Diferença GAM − Lm de ", fm(gam_$R2 - lm_$R2, 3),
      ifelse(gam_$R2 - lm_$R2 > 0.01, ": há curvatura que a reta não capta.", ": pouca curvatura captável; a reta quase empata com as curvas."), "\n",
      "- **Sobreajuste:** maior gap_R2 em ", mx$Metodo, " (", fm(mx$gap_R2, 3), "); Gradient Boosting: ", fm(gb_$gap_R2, 3), ".\n",
      "- **Estabilidade:** menor desvio do RMSE entre repetições em ", est$Metodo, " (", fm(est$RMSE_dp_repeticoes), " kg/ha).\n",
      "- **Independência:** maior piora do RMSE ao deixar parcelas inteiras de fora em ", pior$Metodo, " (+", fm(pior$aumento_RMSE_perc), "%); Gradient Boosting: +",
      fm(ind$aumento_RMSE_perc[ind$Metodo == "Gradient Boosting"]), "%.")

fin <- CFG$metodo_final
resumo <- data.frame(etapa = c("repetições 80/20 (média)", "validação cruzada (média)", "validação 20% (1 partição)", "conjunto II", "conjunto III"),
  R2 = c(sel$R2, tab_cv$R2[tab_cv$Metodo == fin], tab_val$R2[tab_val$Metodo == fin], e2$R2[e2$Metodo == fin], e3$R2[e3$Metodo == fin]),
  RMSE = c(sel$RMSE, tab_cv$RMSE[tab_cv$Metodo == fin], tab_val$RMSE[tab_val$Metodo == fin], e2$RMSE[e2$Metodo == fin], e3$RMSE[e3$Metodo == fin]),
  MAE = c(sel$MAE, tab_cv$MAE[tab_cv$Metodo == fin], tab_val$MAE[tab_val$Metodo == fin], e2$MAE[e2$Metodo == fin], e3$MAE[e3$Metodo == fin]),
  MAPE = c(sel$MAPE, tab_cv$MAPE[tab_cv$Metodo == fin], tab_val$MAPE[tab_val$Metodo == fin], e2$MAPE[e2$Metodo == fin], e3$MAPE[e3$Metodo == fin]))
mostra(cbind(metodo = fin, resumo), paste0("Resumo do método selecionado (", fin, "; preditoras: ", paste(usar, collapse = " e "), "). RMSE e MAE em kg ha⁻¹; MAPE em %."), d = 3); salva(resumo, "resumo_metodo_selecionado")
