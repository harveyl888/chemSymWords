##
## generate chem words
##

elements = c("h","he","li","be","b","c","n","o","f","ne","na","mg","al","si","p","s","cl","ar","k","ca","sc","ti","v","cr","mn","fe","co","ni","cu","zn","ga","ge","as","se","br","kr","rb","sr","y","zr","nb","mo","tc","ru","rh","pd","ag","cd","in","sn","sb","te","i","xe","cs","ba","la","ce","pr","nd","pm","sm","eu","gd","tb","dy","ho","er","tm","yb","lu","hf","ta","w","re","os","ir","pt","au","hg","tl","pb","bi","po","at","rn","fr","ra","ac","th","pa","u","np","pu","am","cm","bk","cf","es","fm","md","no","lr","rf","db","sg","bh","hs","mt","ds","rg","cn","uut","fl","uup","lv","uus","uuo")

chemWord <- function(w, sym=elements) {
  return (chemWordRecurse(w, list(), sym))
}

chemWordRecurse <- local({
  out <- list()
  function(w, t, sym) {
    if (nchar(w) == 0) {
      out[[length(out) + 1]] <<-unlist(t)
    }

    for (i in 1:2) {
      if (substring(w, 1, i) %in% sym) {
        Recall(substring(w, i+1), c(t, substring(w, 1, i)), sym)
      }
    }
    return(out)
  }
})
