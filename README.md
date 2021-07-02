# yaml-with-trust-selection-box

This branch is for separately archiving progress on the transition to a 
YAML-based dashboard up to a specific step: the step where the YAML has been
populated, `app_server.R` has been modified accordingly, **BUT** where the trust
selection box that was used for internal purposes for selecting between trusts 
A, B & C has not yet been removed.

So `yaml-with-trust-selection-box` is essentially a copy of `feature-yaml` up 
to commit [1141993df826dbba1f158d95e25a3d2aa22c1525](https://github.com/CDU-data-science-team/pxtextminingdashboard/commit/1141993df826dbba1f158d95e25a3d2aa22c1525). After this 
commit, `feature-yaml` will not have the trust selection box any more. 
The reasoning  behind making `yaml-with-trust-selection-box` is two-fold:

- Getting rid of the box in `feature-yaml` is a bit of a delicate operation that 
  could break things. Instead of pulling `feature-yaml` from GitHub in case 
  things break locally, I much prefer to have the code archived on its 
  own;
- We may want to use code for selection boxes in the future, so a dedicated 
  branch makes it much easier to recover such code;
