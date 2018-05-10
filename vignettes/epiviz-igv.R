library(epivizrChart)

epiviz_env <- epivizEnv(chr="chr11", start=118000000, end=121000000)

genes_track <- epiviz_env$plot(Homo.sapiens)

blocks_track <- epiviz_env$plot(tcga_colon_blocks, datasource_name="450kMeth")

epiviz_igv <- epiviz_env$plot(file="https://s3.amazonaws.com/igv.broadinstitute.org/annotations/hg19/genes/refGene.hg19.bed.gz",
                         file_type="annotation", file_format = "bed", datasource_name = "genes2",
                         chr="chr11", start=118000000, end=121000000)

epiviz_env

# epivizigv
# epivizNav <- epivizNav(chr="chr11", start=118000000, end=121000000, interactive=TRUE)

