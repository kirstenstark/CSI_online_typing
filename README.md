# Web-based language production experiments: Semantic interference assessment is robust for spoken and typed response modalities
## Experiment 2: CSI with typewritten picture naming

The project provides data and analyses scripts of doi:  10.3758/s13428-021-01768-2

The study serves as a proof of principle demonstration that (a) language production experiments can be run online, that (b) a cumulative semantic interference (CSI) effect can be elicited in typed picture naming, and that (c) automated preprocessing procedures can further reduce the workload in language production experiments. In a first study, we replicated an effect that is well-established in the laboratory, the spoken CSI effect in a continuous picture naming paradigm, in an online setting on participants' web browsers. In this study, we aimed at replicating the spoken CSI effect in typed picture naming. The replication of the CSI effect in keystroke latencies can facilitate data collection and preprocessing in future continuous picture naming experiments.
The study was coded in SoSciSurvey (Leiner, 2019) and run in participants’ web browser. Participants were recruited via Prolific (https://www.prolific.co), and analyses were performed in R. 
**Custom JavaScript code was used to detect keystroke latencies** ([Stark, 2021; https://github.com/kirstenstark/typing_RTs_JS](https://github.com/kirstenstark/typing_RTs_JS)). R functions for **automated preprocessing** are provided ([Stark, 2021; https://github.com/kirstenstark/stringmatch_typed_naming](https://github.com/kirstenstark/stringmatch_typed_naming)). 

[R scripts](https://github.com/kirstenstark/CSI_online_typing/tree/main/scripts/code) and [outputs](https://github.com/kirstenstark/CSI_online_typing/tree/main/scripts/github), as well as [anonymized data](https://github.com/kirstenstark/CSI_online_typing/tree/main/data) are provided.  
Package versions are saved using the renv-package (Ushey, 2020). 



### License
This work was created by Kirsten Stark at the Humboldt-Universität zu Berlin and is subject to the [MIT License](https://github.com/kirstenstark/CSI_online_typing/blob/main/LICENSE).
