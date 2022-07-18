This is a web interface to the open-source R package <a href="https://cran.r-project.org/package=gMCPLite" target="_blank">gMCPLite</a> for multiple testing using graphical approach built with Shiny.

### Features

**Create an initial multiplicity graph**

- Designs for multiple endpoint types: time-to-event, binomial, normal, as well as the ability to extend a fixed design sample size to a group sequential design.
- Information-based design.
- Flexible timing and number of interim analyses.
- Spending function boundaries with a variety of flexible spending functions.
- For time-to-event (proportional hazards) designs, users can specify various enrollment rates, dropout rates, and event rates. Based on these inputs, expected calendar times for analyses are available.
- Tabular output, which can be copied into a word processor, is supplied as well as a brief textual summary of a design.
- Several plots describing designs are available.
- R code that allows you to save hGraph code that can be re-run to reproduce your multiplicity graph at a later date.

**Iterative graphs update**

- Update bounds at time of analysis.
- Flexibile number of total analyses.
- Alpha spending strategies for the final analysis and interim analyses.
- Tabular output and report generator.

**Save and restore design**

- Save and restore all parameters when creating the design or updating the bounds.

### Contributors

Creator & Project Manager & Maintainer

- Yalin Zhu, PhD \<yalin.zhu at merck.com\>

Architect Expert

- Nan Xiao, PhD \<nan.xiao1 at merck.com\>

Strategic Advisor

- Keaven Anderson, PhD \<keaven_anderson at merck.com\>

Developers

- Xuan Deng, PhD \<xuan.deng at merck.com\>
- Nan Xiao, PhD \<nan.xiao1 at merck.com\>
- Yalin Zhu, PhD \<yalin.zhu at merck.com\>


### License

gMCPShiny is licensed under <a href="https://cran.r-project.org/web/licenses/GPL-3" target="_blank" title="License information">GPL >= 3</a>.
