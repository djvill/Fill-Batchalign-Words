# TODO

- Add 'loading spinners' with [`shinycssloaders`](https://cran.r-project.org/web/packages/shinycssloaders/index.html)
- Separate the generation of `wordDF` and cache the result (so the user can generate versions w/ different settings w/o waiting for time-consuming splitWords() call every time)
- The "Mimic call requirement" step is hacky---figure out whether it's necessary, and if so, a better way to implement it
- If `mergeMax` is finite, there's no `File` column in the output of `processBatchalign()`, but if `mergeMax=Inf`, the `File` column is retained
