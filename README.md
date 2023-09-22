# pxtextminingdashboard: Text Mining Dashboard of Patient Experience Feedback

Nottinghamshire Healthcare NHS Foundation Trust hold thousands of patient feedback 
records that contain invaluable comments about what was best, what went wrong and 
how services could be improved. Identifying what most patients talk about, and 
what the general sentiment about the services is, are key variables in helping 
decision-makers (e.g. ward managers) understand, prioritize and act upon any issues.

Reading through all patient feedback comments, identifying issues and summarizing
them in a way that can aid decision-making, can be time-consuming and requires 
several staff hours. 

**How can we take some work off the shoulders of staff? And how can we be confident 
that we identify burning issues in a timely manner?**

We can automate much of the process with the power of algorithms: *Text Mining* 
"[...] *is the process of transforming unstructured text into a structured format 
to identify meaningful patterns and new insights.*" (IBM, 2021). In this project,
we use text mining to efficiently process thousands of patient feedback text 
comments. We feed the results of our analysis into an interactive dashboard to 
translate complex findings into illuminating and actionable insights.

The interactive dashboard can be shipped as an [`R`](https://www.r-project.org/) 
package and it comes with real open-source patient feedback text records for 
demonstration purposes. A key feature though is that it can be used with *any* 
text dataset from *any* organisation. We are offering the possibility of hosting 
the dashboard for different text data on our server. Here is an [example](https://feedbackmatters.uk/rsconnect/text_mining_dashboard/) with our 
own data.

## Dashboard structure
The dashboard focuses on three main areas of text mining:

1. [Text Classification](#text-classification);
2. [Sentiment Analysis](#sentiment-analysis);
3. [Further text processing](#further-text-processing), such as word and *n-gram* 
   frequencies;

A more detailed explanation of these three areas is provided below for users who
are not necessarily familiar with these concepts. However, there are also several
red boxes above/below summarised results (tables and plots) on the dashboard that
briefly explain concepts and provide useful links.

### Text Classification
*Text Classification* is an area of [Machine Learning](https://en.wikipedia.org/wiki/Machine_learning) where algorithms learn 
from labelled text and are subsequently used to predict the labels of unlabelled 
text. 

Consider, for example, that feedback is labelled under a number of themes 
such as "Staff", "Communication", "Environment" and "Privacy & Dignity", among 
others. We can feed the text and their themes to an algorithm that will learn to 
distinguish between these themes based on the content of the text. When new 
feedback text enters the system, like "The ward staff were lovely.", then the 
algorithm will label it as "Staff".

We need to acknowledge that no algorithm can get it right 100% of the time. What
would the algorithm do with a comment like "The ward staff were very keen to 
communicate the process in a friendly manner."? Should it be labelled as "Staff" 
or as "Communication"? Accepting that such errors are unavoidable, what matters
more in practice is that we can at least correctly label much of the text to 
enhance the *discoverability* of feedback that is concerned with a particular 
topic. More clearly, a ward manager interested in knowing about "Staff", who is 
faced with hundreds of comments that are yet to be labelled, would benefit from 
a text classification model that filters out unlabelled text that has been 
predicted to be about themes other than "Staff".

#### Text classification on the dashboard
Text classification on the dashboard consists of two tabs. One tab presents a 
table with the unlabelled text and the predicted labels. The other tab is more 
for technical people who would like to assess model performance in a variety of
ways (confusion matrix, performance metrics, predictions on test data etc.).

It is worth mentioning that the confusion matrix is a useful tool for understanding
where the model "gets it wrong" most of the time. The confusion matrix shows how 
many times the model predicted a theme correctly or incorrectly. Consider a 
model that correctly predicts most comments that are about "Staff", and incorrectly
predicts the remaining comments that are about "Staff" as being about "Communication".
These two themes are not far off, and a ward manager would most likely be 
interested in reading feedback about "Communication" too if they are interested 
in the "Staff" category. So we would conclude that the errors that this model is 
making are not that dramatic. On the other hand, if it predicted the remaining 
texts that are about "Staff" as being about "Food", that would be stronger 
evidence that the model is not performing well.

### Sentiment Analysis
*Sentiment Analysis* is concerned with extracting the sentiment that is expressed
in a text. Is the patient sad or angry about something? Are patients generally 
positive or negative about a particular service? 

There are several ways of extracting sentiment from text, ranging from fairly 
simple to quite complex. One way to go is with the use of *sentiment lexicons* 
(Silge & Robinson, 2017, [section 2.1](https://www.tidytextmining.com/sentiment.html#the-sentiments-datasets). See
also Hu & Liu, 2004, Mohammad & Turney, 2013, and Nielsen 2013.).
These are lexicons that process large amounts of text from a variety of resources
in order to empirically determine the sentiment expressed in a word. Once the 
text has been broken into tokens and a sentiment has been assigned to each token 
(where applicable- it obviously makes no sense to assign a sentiment to the word 
"chair"!), one can calculate all sorts of summary statistics, like the number of 
times an angry or happy sentiment was expressed in the evaluated text.

Sentiment analysis is far from perfect. There are so many ways of expressing 
ourselves in writing that it can be quite challenging to correctly capture the 
true feelings of a person. It is particularly hard to deal with colloquialisms, 
sarcasm, irony and negation, among others. Although the sentiment analysis on 
the dashboard suffers from these limitations, it is still extremely useful in
helping discover key areas of concern or success for the Trust.

#### Sentiment analysis on the dashboard
The dashboard presents sentiment analysis results for two different levels:

1. **Theme-level sentiment analysis.** Here, sentiment from all feedback texts 
   is summarised for each theme (e.g. "Staff", "Communication", "Environment" 
   etc.) in two ways, and for two reasons:
   - Identify the most positive or negative words used in the text that is about
     a particular theme;
   - Sort the themes in terms of the "net sentiment" expressed in them, that is,
     the number of words with positive sentiment minus the number of words with 
     negative sentiment in the feedback text;
2. **Text-level sentiment analysis.** Here, the idea is to analyse the sentiment
   in each feedback text individually. For example, we count the number 
   of times that different sentiments are expressed in a feedback comment (e.g. 
   is the patient mostly angry, sad or happy about something?). We also use 
   algorithms that "score" feedback text to provide an aggregate indicator or 
   positivity or negativity in the text;

### Further text processing
Other useful concepts in text mining are *term frequency-inverse document frequency* 
(TF-IDF; Silge & Robinson, 2017, [chapter 3](https://www.tidytextmining.com/tfidf.html#tfidf)) 
and *n-grams* (Silge & Robinson, 2017, [section 4.1](https://www.tidytextmining.com/ngrams.html#tokenizing-by-n-gram)):

1. **TF-IDF.** This is a simple indicator that helps surface the most prevalent 
   content in the text. It can be used to count how many times a word occurs in 
   the text (*term frequency*), however this count is multiplied by the 
   *inverse document frequency* (the natural logarithm of the number of feedback 
   texts divided by the number of text records containing the word), to decrease 
   the weight for commonly used 
   words and increase the weight for words that are more rarely used in a 
   collection of feedback texts. Thus, TF-IDF is the frequency of a term 
   adjusted for how rarely it is used, and therefore measures the importance of 
   a word in a collection of feedback texts. Interestingly, TF-IDF need not be 
   necessarily calculated for single words, but for *n*-grams too, which are 
   defined below;
2. **_n_-grams.** These are sequences of *n* consecutive words. For `n = 1`, 
   we get *uni-grams*, which are single words (e.g. "excellent"). For `n = 2` 
   and `n = 3` we get *bi-grams* and *tri-grams* respectively (e.g. "excellent 
   service" and "excellent service, thanks" respectively).
   
#### Further text processing on the dashboard
The dashboard summarizes TF-IDF and *n*-gram analysis in two ways:
1. TF-IDFs bar plots of uni-grams and bi-grams for each theme to identify what 
   patients mostly talk about, e.g. "listened carefully" for "Communication";
2. Network diagrams of bi-grams, that is, a visual representation of bi-grams 
   with arrows pointing from the first to the second word. Network diagrams offer
   an alternative way to the TF-IDF bar plots of visually identifying the most 
   commonly mentioned subjects;

## References
IBM (2021). Text Mining [Online]. Available at: 
https://www.ibm.com/cloud/learn/text-mining (accessed 28 June 2021).

Hu M. & Liu B. (2004). Mining and summarizing customer
reviews. Proceedings of the ACM SIGKDD International Conference on
Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA,
Aug 22-25, 2004.

Mohammad S.M. & Turney P.D. (2013). Crowdsourcing a Word–Emotion
Association Lexicon. Computational Intelligence, 29(3):436-465.

Nielsen F.A. (2013). A new ANEW: Evaluation of a word list for
sentiment analysis in microblogs. Proceedings of the ESWC2011 Workshop
on 'Making Sense of Microposts': Big things come in small packages 718
in CEUR Workshop Proceedings 93-98. https://arxiv.org/abs/1103.2903.

Silge J. & Robinson D. (2017). Text Mining with R: A Tidy Approach. Sebastopol, 
CA: O’Reilly Media. ISBN 978-1-491-98165-8.
