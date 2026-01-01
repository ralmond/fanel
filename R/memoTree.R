memoTree <- R6Class(
  "memoTree",
  public=list(
    depth=1L,
    initialize=function(depth=1L) {
      self$depth <- depth
      private$cache=new.env(parent=emptyenv())
    },
    exists=function(key){
      if (length(key)>self$depth) {
        stop(paste("Key must be of length ",self$depth,"."))
      }
      key <- as.character(key)
      if (!exists(key[1],envir=private$cache)) return(FALSE)
      if (length(key)==1L) return(TRUE)
      get(key[1],envir=private$cache)$exists(key[-1])
    },
    get=function(key){
      if (length(key)!=self$depth) {
        stop(paste("Key must be of length ",self$depth,"."))
      }
      key <- as.character(key)
      if (!exists(key[1],envir=private$cache)) return(NULL)
      val <- get(key[1],envir=private$cache)
      if (length(key)==1L) return(val)
      val$get(key[-1])
    },
    assign=function(key,value) {
      if (length(key)!=self$depth) {
        stop(paste("Key must be of length ",self$depth,"."))
      }
      key <- as.character(key)
      if (length(key)==1L) {
        assign(key,value,envir=private$cache)
      } else {
        if (!exists(key[1],envir=private$cache)) {
          nextt <- memoTree$new(length(key)-1L)
          assign(key[1],nextt,envir=private$cache)
        } else {
          nextt <- get(key[1],envir=private$cache)
        }

        nextt$assign(key[-1],value)
      }
    },
    clear=function() {
      if (self$depth >1L) {
        lapply(ls(envir=private$cache),\(key) {
          get(key,envir=private$cache)$clear()
        })
      }
      rm(list=ls(envir=private$cache),envir=private$cache)
    }
  ),
  private = list(
    cache=NULL
  )
)
