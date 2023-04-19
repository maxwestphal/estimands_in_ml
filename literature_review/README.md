# Estimands in machine learning algorithm evaluation

------------------------------------------------------------------------

## Literature review

In the literature review … analysis of how estimands are currently
specified in the applied machine learning literature

## Pubmed extraction

what are the scripts doing…

-   literature\_review\_funs.R
-   literature\_review\_init.R
-   literature\_review\_prep.R
-   literature\_review\_extract.R
-   literature\_review\_annotation.R: The file
    literature\_review\_annotation produces a folder “annotation” within
    the files folder.

## Technical preparation

Within the created folder estimands\_in\_ml/annotation the pubmed
results are stored. The annotation workflow, consists of the following
steps:

1.  copy annotation folder and save as **annotation\_xy/** where xy
    indicates the annotators (i.e. your) initials.
2.  Within the **annotation/** folder, there is one subfolder for each
    year included in the analysis, i.e. **annotation/yyyy/**
3.  For each each included year folder, there are two subfolders:
4.  **annotation/yyyy/match**
5.  **annotation/yyyy/nomatch**

Both folder **match** and **nomatch** include CSV files to be annotated.
An identical second version (“…\_2.csv”) only deviates by the column
delimiter, ONLY ONE FILE, *annotation\_yyyy\_match\[no\].csv* OR
*annotation\_yyyy\_\[no\]match\_2.csv*

## Annotation workflow

The goal of this review is to judge the empirical estimand of each
study. (Exception: if outlook\_only = yes, then the proposed estimand
for future work.) What parts should be read for each paper? Abstract
-&gt; Methods -&gt;Tables/Figures -&gt; Discussion/conclusion (-&gt;
potentially other parts) (~5 minutes per paper max) The following fields
should be annotated (first two only for nomatch folder). For all fields,
the default value is “” (empty cell).

### Basic info

-   exclude (default: no | 1: yes): should the entry be excluded?
    (judged based on the title and abstract only, target population of
    papers: development and/or validation of clinical risk prediction
    models)
-   no\_pdf (default: no | 1: yes): is there no PDF full text available?
    Categorization of study
-   outlook\_only (default: no | 1: yes): is the estimand also estimated
    in the present study or only proposed in the outlook?
-   val\_only (default: no | 1: yes): is the study only dealing with an
    external (model/algo) validation, i.e. was the model/algo
    development carried out in an earlier study?
-   study\_design (default: NA| free text otherwise) specification of
    the study\_design (e.g. “leave-one-clinic out”, “custom CV”, random
    T-V-E split) for the discussed estimand +++THIS IS AN OPTIONAL FIELD
    to gather potential examples+++

### Categorization of estimand

-   unconditional (default: no | 1: yes): is the paper concerned with
    model evaluation (conditional on training data; i.e. single training
    run (per model); the default) or algorithm evaluation (training
    repeated over different training datasets; unconditional = 1)?
-   estimand\_rdm (default: no | 1: yes) the empirical estimand is not
    structured but rather relies on random data splitting
-   estimand\_vague? (default: no | 1: yes) the evaluation involves
    external data (another cohort) without exact specification what the
    empirical estimand is (i.e. none of the descriptions below apply)
-   transport\_site (default: no | 1: yes): is the estimand concerned
    with transportability to different (clinical) sites?
-   transport \_region (default: no | 1: yes): is the estimand concerned
    with transportability to different regions (e.g. countries)?
-   transport \_time (default: no | 1: yes): is the estimand concerned
    with transportability to different time periods?
-   transport \_process (default: no | 1: yes): is the estimand
    concerned with transportability to different processes
    (e.g. measurement devices/protocols).
-   transport \_setting (default: no | 1: yes): is the estimand
    concerned with transportability to different settings (e.g.,
    intensive care to normal care unit) or population / covariate shift
    (e.g., adults to children)?
-   overlap (default: no | 1: yes) does (partial) overlap occur between
    different estimand aspect (i.e., between transport\_xyz fields)?
    “no” implies that estimand aspects can be clearly distinguished in
    the study/data sample. “yes” implies that this is not the case
    (e.g., one hospital for each of 5 countries, that is the
    transportability between regions and between hospitals cannot be
    distinguished). Should be set to 1/“yes” even if this problem only
    occurs partially (e.g. 3 clinics in Germany, 1 clinic in
    Switzerland, 1 clinic in Italy). If overlap is 1/”yes”, at least 2
    transpost\_xyz fields should be 1/”yes”. If needed, details can be
    explained in “remarks” field.

### Other fields

-   remark (default: NA | free text otherwise): any remark from the
    annotator (e.g. details on estimand or estimator definition)
-   done (default: no | 1: yes): mostly meant as a progress tracker for
    the annotator.

## Remarks on annotation workflow

Exactly one of these 3 cases should apply to every study (1)
estimand\_rdm = 1, estimand\_vague = “”, all transport\_xyz fields = “”
(2) estimand\_rdm = “”, estimand\_vague = 1, all transport\_xyz fields =
“” (3) estimand\_rdm = “” and estimand\_vague = “” and one or multiple
transport\_xyz fields are set to 1/”yes”

Q: What transport\_xyz fields should be marked in this case? In
particular in a situation of (partial) separation (e.g. one hospital for
each of 5 regions (e.g. countries)), that is the transportability
between regions and between hospitals cannot be distinguished.)? A: all
that apply (explicitly or implicitly). In the example case, the
separation variable should also be set to 1/”yes”.

In case multiple estimands are described in the study, this should be
specified as a remark. All aspects/dimensions considered by any of the
estimands should be annotated as such. In this case, the overlap field
should be “yes”, if this is true for one of the estimands.

If a publication is excluded but relevant from a methodological
perspective, please annotate as exclude=1 and add a corresponding
remark, e.g. “relevant methodological work”.
