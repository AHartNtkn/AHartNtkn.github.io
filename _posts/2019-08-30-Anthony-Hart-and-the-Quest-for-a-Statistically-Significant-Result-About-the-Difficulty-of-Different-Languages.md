# !!!!!!!!~(DRAFT)~!!!!!!!!

What's the most difficult language to learn? The answer will depend on what languages you know already, but since I'm writing this in English, let's assume that's all you know. In that case, there's a handy database maintained by the [Foreign Service Institute](https://www.atlasandboots.com/foreign-service-institute-language-difficulty/) which tells you how long it takes for a native English speaker to learn a variety of languages. It's hardly exhaustive, but it includes just about every language that could be considered "major".

The answer to the initial question is Japanese, for the record; restricted to the languages within that database, of course. But, why? There are lots of things one could point to; its morphology, it's grammar, it's semantics. But there are other hard languages as well; Arabic is notoriously hard to learn, and yet it's very different from either Japanese or English. What things, truly, make a language hard to learn? That's what this post is going to explore.

- [Boilerplate](#heading1)
- [Seeing The World](#heading2)
- [Preliminary Examination](#heading3)
- [Thorough Examination](#heading4)
- [Goodbye!](#heading5)

<a name="heading1"></a>
## Boilerplate

Throughout this post, I will be making use of two datasets. Firstly, [this](https://data.world/dataremixed/language-difficulty-ranking) version of the FSI data put into a nice csv file, and secondly, [this](https://wals.info/download) database version of the Atlas of Language Structures. With these, one might be able to draw conclusions about what makes languages more or less difficult to learn.

Firstly, some housekeeping! For reference, I will be using Python and the following libraries for my analysis.

```python
import matplotlib.pyplot as plt
import pandas as pd
import geopandas as gpd
import numpy as np
from scipy.stats import ttest_ind, chi2_contingency
```

The two datasets aren't completely compatible; as such, some changes need to be made to combine them. I will call the initial difficulty dataframe `diffDf`, and rename a few entries to make it consistent the ALS dataframe.

```python
# Import and clean database of language difficulties.
diffDf = pd.read_csv("3r.csv")
diffDf = diffDf.drop(['category', 'title', 'description'], axis=1)
# Fix the two oddly formated entries
diffDf.loc[65,'language'] = 'Cantonese'
diffDf.loc[66,'language'] = 'Mandarin'
# Fix languages with different names between databases
diffDf.loc[24,'language'] = 'Persian' # Was Farsi
# Note: Kyrgyz is on the diff list but not the lang list [37]
diffDf.loc[42,'language'] = 'Khalkha' # Was Mongolian; Khalkha is the only dialect of Mongolian on the lang list.
diffDf.loc[47,'language'] = 'Serbian-Croatian' # was 'Serbo-Croatian'
diffDf.loc[50,'language'] = 'Slovene' # was 'Slovenian'
diffDf.loc[53,'language'] = 'Tajik' # was 'Tajiki'
diffDf.loc[60,'language'] = 'Ukrainian' # was 'Ukranian'
```

I will then load the ALS dataset into a dataframe called `landDf`.

```python
langDf = pd.read_csv("language.csv")
```

The ALS dataset includes many languages, several of which are dialects of other languages. Whenever a language is a dialect of another language, it's denoted "Language (Dialect)". For instance, Egyptian is listed as "Arabic (Egyptian)". The difficulty dataset isn't as granular. I decided to assign each dialect the difficulty of the parent language listed in the FSI dataset.

One might ask at this point, is it a principled to assume that every dialect of a language has equal difficulty of learning? No, probably not...

In order to pull off the merge, I wrote a function to detect if a language from the ALS database appears in the FSI database.

```python
# If a language is in the difficulty table, say where
def present(x):
  result = -1
  for i in diffDf.T:
    if x.startswith(diffDf['language'][i]):
      result = i
  return result
```

I then systematically filter the ALS database, removing languages that don't have difficulties and adding columns for learning time for everything else.

```python
# Fill in the time, accounting for dialects
for i in langDf.T:
  checkPresence = present(langDf['Name'][i])
  if checkPresence == -1:
    langDf = langDf.drop(i)
  else:
    langDf.loc[i, 'weeks_to_achieve_goal'] = diffDf['weeks_to_achieve_goal'][checkPresence]
    langDf.loc[i, 'class_hours'] = diffDf['class_hours'][checkPresence]

langDf = langDf.reset_index()
```

<a name="heading2"></a>
## Seeing The World

The ALS dataset contains country codes for each languages' country of origin. With this, the difficulty of languages can be plotted across the world. However, they aren't in the right format. Some of the languages list multiple country codes. To fix this, I must create a new dataframe with the individual country codes connected to the difficulties.

I start out with a dictionary that I can use to map country codes to languages difficulty. For each listed language and for each country code listed, if the country isn't already in the dictionary, add it along with its difficulty; if it is, update the mean difficulty.

```python
count_dict = {}

for i in langDf.T:
  newKeys = langDf['countrycodes'][i].split()
  newHours = langDf['class_hours'][i]
  for key in newKeys:
    if key in count_dict.keys():
      count_dict[key] = ((count_dict[key][0]*count_dict[key][1]+newHours)/(count_dict[key][1]+1), count_dict[key][1]+1)
    else:
      count_dict[key] = (newHours, 1)
```

I now convert the dictionary into a new dataframe by first converting it into an array, then casting it.

```python
newDF = [ [key,count_dict[key][0]] for key in count_dict.keys() ]

count_hours = pd.DataFrame(newDF,  columns=['countrycodes','class_hours'])
```

A bit annoying, but not too hard. But now I need to plot this out. I need a shapefile for the earth. A good source is [naturalearthdata.com](https://www.naturalearthdata.com). In particular, the [100m data](https://www.naturalearthdata.com/downloads/110m-cultural-vectors/) will work fine for my purposes. I will load this data using geopandas. This will give me a large database with lots of columns. One of them, the main geometric data, consists of polygons describing the geometry of various countries. The other column I need is the `ISO_A2` column, being one of several columns giving country codes. That one contains the ISO alpha-2 country codes, the same codes that the ALS database uses. 


```python
shapefile = 'ne_110m_admin_0_countries.shp'

gdf = gpd.read_file(shapefile)[['ISO_A2', 'geometry']].to_crs('+proj=robin')
```

Also, France and Norway's ISO alpha-2 codes are listed as `'-99'` for some reason, so that needs to be fixed.

```python
gdf['ISO_A2'][43]='FR'
gdf['ISO_A2'][21]='NO'
```

These can now merged together.


```python
merged = gdf.merge(count_hours, left_on='ISO_A2', right_on='countrycodes', how = 'left').drop(159)
```

Row 159 contains Antarctica. Since it just takes up space on my map, I decided to simply drop it.

At this point, I can get on with plotting. In particular, I'll be plotting two layers. The first layer will plot the avaliable data, the second will plot any country who's data is missing.


```python
fontColor = "#a61717"

ax = merged.dropna().plot(column='class_hours', cmap='Greens', figsize=(16, 10), scheme='equal_interval', k=8, legend=True)

merged[merged.isna().any(axis=1)].plot(ax=ax, color='#aaaaaa', hatch='//')
ax.set_title('Hours Needed to Learn Language by Country of Origin (Averaged)', fontdict={'fontsize': 20}, loc='left', color=fontColor)

ax.set_axis_off()
ax.set_xlim([-1.5e7, 1.7e7])
leg = ax.get_legend()
leg.set_bbox_to_anchor((.12, .4))
leg.set_alpha(0)
leg.set_frame_on(False)

niceLabels = ["600-800", "800-1000", "1000-1200", "1200-1400", "1400-1600", "1600-1800", "1800-2000", "2000-2200"]
i=0
for text in leg.get_texts():
    text.set_text(niceLabels[i])
    i+=1
    text.set_color(fontColor)

ax.get_figure();
```

![Hours Needed to Learn Language by Country of Origin (Averaged)](../img/AHQSSRADDL/language_difficulty.png)


Shoutout to Ramiro Gómez and his [helpful tutorial](https://ramiro.org/notebook/geopandas-choropleth/). So many choropleth guides overcomplicate things with interactivity and way too many extraneous libraries, but I found this one to be just right.

From this plot, a few things can be noticed. The hardest languages are concentrated in two places; firstly in the far east within Japan and Korea, and secondly spread throughout northern Africa and the Middle East. That second area is so dark mostly due to different dialects of Arabic. It can also be noted that the easiest languages tend to group in Europe, close to England. Not surprising, as these are where the languages most similar to English are concentrated.

There is also a lot of missing data. Few major languages come from the Americas, Australia, or southern Africa, and, as a result, information on the difficulty of languages from there isn't as easy to find. Or, at the very least, the FSI didn't bother to find it. 

<a name="heading3"></a>
## Preliminary Examination

Which structures are most important when it comes to learning a new language? There are a couple of things that come to mind, the sort of thing people complain about when learning a new language; the writing system, conjugation, the vocabulary, etc. These things appear in the guise of various structures within the ALS database. One of the more interesting of these is grammatical gender; let's start with that.

The most obvious thing to start with is simply the number of genders in the language. Some languages have over a dozen, while many have none. One might imagine that the more exotic gender systems are harder to learn. This can be plotted to find out.

<div style="text-align:center" TITLE="Hours Needed to Learn Language by Number of Genders"><img src="../img/AHQSSRADDL/num_of_genders.png" /></div>

<!-- 
ax = langDf.pivot_table(values=['class_hours'], index='30A Number of Genders').sort_values('class_hours').plot(kind='bar', color='#8ED08B');

fontColor = "#a61717"

plt.autoscale(tight=True)

ax.set_frame_on(False)

ax.set_xticklabels(["5+", "3", "0", "2"], rotation=0, color=fontColor)
ax.set_yticklabels(["0", "200", "400", "600", "800", "1000", "1200"], color=fontColor)

plt.tick_params(
    axis='both',
    which='both', 
    bottom=False,
    top=False,
    left=False,
    labelbottom=True)

ax.set_xlabel('')

plt.title("Hours Needed to Learn Language by Number of Genders", color=fontColor);
ax.get_legend().remove()
plt.show()
 -->

Interesting. The easiest category is the one with five or more genders. That's quite unexpected. Out of curiosity, I looked at which languages were in this category. As it turns out, there was only one in my list, Swahili. It is apparently fairly easy to learn for English speakers, taking around 900 hours.

Swahili has an interesting system with around 10 genders, depending on how you count them. Some are pretty ordinary, making distinctions between animate and inanimate objects, while some are quite peculiar, such as a gender for artifacts and tools. Interestingly, there aren't equivalents of masculine/feminine gender. If you're interested, [here's](https://en.wikipedia.org/wiki/Swahili_grammar) a rundown:

But, what conclusion should be drawn from this? Not much, the trend seems to be a coincidence. There just aren't many major languages with complex gender systems. As a consequence, data about how hard they are to learn isn't as easy to come by.

I also think the data at the Atlas of Language Structures might be a bit faulty; it's certainly incomplete besides. English is listed as having three genders; masculine, feminine, and neuter. I don't think many people would recognize this. Nowadays, there are few gendered words (actor vs. actress, dominator vs. dominatrix), and their nature is semantic more than grammatical. There is an argument to be made that, for most of its history (up to around the 1600s), gender was a clear part of English, wherefrom many gendered artifacts come.

The only place where gender plays a clear role in English is in pronoun usage. This made me think, what about Japanese? It uses gender in a way that's very similar to English. For the most part, it doesn't really have gender, though it has a few words that are semantically distinguished by sex (海人 (fisherman) and 海女 (fisherwoman), for instance), such examples are relatively rare. Like English, it also genders some of its pronouns, such as 彼女 (she) and 彼 (he). Unlike English, it even has gendered personal pronouns such as 僕 (I, masculine). On top of that, Japanese does grammatically care about things like animate vs. inanimate nouns, something often implemented via grammatical genders. So I looked up Japanese in the atlas to see how many genders it listed. As it turns out, that spot is simply blank. ¯\\_(ツ)_/¯ Japanese' gendered pronouns, however, are mentioned in a separate column. Graphing that;

<div style="text-align:center" TITLE="Hours Needed to Learn Language by Gender Distinctions in Independent Personal Pronouns"><img src="../img/AHQSSRADDL/num_of_genders_pronouns.png" /></div>

<!-- 
ax = langDf.pivot_table(values=['class_hours'], index='44A Gender Distinctions in Independent Personal Pronouns').sort_values('class_hours').plot(kind='bar', color='#8ED08B');

fontColor = "#a61717"

plt.autoscale(tight=True)

ax.set_frame_on(False)

ax.set_xticklabels(["No gender distinctions", "3rd person singular only", "In 3rd person + 1st and/or 2nd person", "3rd person only, but also non-singular"],
                    rotation=10, color=fontColor)
ax.set_yticklabels(["0", "200", "400", "600", "800", "1000", "1200"], color=fontColor)

plt.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom=False,      # ticks along the bottom edge are off
    top=False,         # ticks along the top edge are off
    left=False,         # ticks along the top edge are off
    labelbottom=True) # labels along the bottom edge are off

ax.set_xlabel('')

plt.title("Hours Needed to Learn Language by\nGender Distinctions in Independent Personal Pronouns", color=fontColor);
ax.get_legend().remove()
plt.show()

-->

Interestingly enough, there isn't much of a difference in class hours between the categories. That highest category does include Japanese, along with various other languages such as Polish and Swahili. There doesn't seem to be much sense in which languages which are similar use gender in a similar way, causing few correlations in the end.

I'm drawn to testing the significance of these, admittedly weak, correlations.

```python
c = '44A Gender Distinctions in Independent Personal Pronouns'
t = ttest_ind(langDf[langDf[c]=='6 No gender distinctions']['class_hours'],
          langDf[langDf[c]=='2 3rd person only, but also non-singular']['class_hours'])
print("t-statistic: ", t.statistic, "\np-value: ", t.pvalue)
```

    t-statistic:  -1.400262210171068 
    p-value:  0.18052782358880434

As, perhapse, expected, they are not significant. Furthermore, I went through a bunch of different attributes, from subject, object, verb order to negation morphology, and I couldn't find a single significant result. Hmm, what to do...

<a name="heading4"></a>
## Thorough Examination

Well, let's stop looking manually, and do things automatically! I wrote this code to perform every t-test possible on all values for every column. It first prints a graph of the data, then it makes a table of t-tests for each combination of values.

```python
for c in langDf.columns:
  if not (c in [ 'index', 'wals_code', 'iso_code', 'glottocode'
               , 'Name', 'latitude', 'longitude', 'countrycodes' ]):
    langDf.pivot_table(values=['class_hours'], index=c).sort_values('class_hours').plot(kind='bar');
    plt.title(c)
    plt.show()

    with pd.option_context('display.max_rows', 10, 'display.max_columns', 7):
      print(
        pd.DataFrame(
          [ [ ttest_ind( langDf[langDf[c]==r1]['class_hours']
                       , langDf[langDf[c]==r2]['class_hours']).pvalue
              for r1 in langDf[c].unique()[1:] ]
              for r2 in langDf[c].unique()[1:] ]
          , columns=langDf[c].unique()[1:]) )
```

With this, I can simply look for small p-values! Of course, let's consider the fact that this is doing over 100 t-tests. If I cared about p<.05 results, I would expect there to be several false positives. To be more restrictive, I'll only look for p<.01 results.

The first thing I find is the relation between languages with different words for tea.

<div style="text-align:center" TITLE="Hours Needed to Learn Language by Word for Tea"><img src="../img/AHQSSRADDL/word_for_tea.png" /></div>

<!-- 
ax = langDf.pivot_table(values=['class_hours'], index='138A Tea').sort_values('class_hours').plot(kind='bar', color='#8ED08B');

fontColor = "#a61717"

plt.autoscale(tight=True)

ax.set_frame_on(False)

ax.set_xticklabels(['From Min Chinese "te"', "Other", 'From Sintic "cha"'], color=fontColor, rotation=0)
ax.set_yticklabels(["0", "200", "400", "600", "800", "1000", "1200"], color=fontColor)

plt.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom=False,      # ticks along the bottom edge are off
    top=False,         # ticks along the top edge are off
    left=False,         # ticks along the top edge are off
    labelbottom=True) # labels along the bottom edge are off

ax.set_xlabel('')

plt.title("Hours Needed to Learn Language by Word for Tea", color=fontColor);
ax.get_legend().remove()
plt.show()
-->

<!-- 
```python
c = '138A Tea'
t = ttest_ind(langDf[langDf[c]=='1 Words derived from Sinitic cha']['class_hours'],
          langDf[langDf[c]=='2 Words derived from Min Nan Chinese te']['class_hours'])
print("t-statistic: ", t.statistic, "\np-value: ", t.pvalue)
```
-->

    t-statistic:  3.226432025596896 
    p-value:  0.002113020067139584

It's unlikely that having "Cha" as the word for tea makes a language harder. I suspect that this has more to do with history/location than anything. To see this, one can run a χ² test with this vs language genus.

```python
ct = pd.crosstab(langDf['genus'], langDf['138A Tea'])
chi_squared, p_value, dof, expected = chi2_contingency(np.array(ct))
print('Chi-squared: ', chi_squared, '\np-value: ', p_value)
```

    Chi-squared:  78.91874999999999 
    p-value:  0.03526750602855796

It seems most likely that those languages which happen to be hard for English speakers often share a history which is what really determines this category.

Something that I'd expect to genuinely contribute to difficulty is the handling of nominal vs verbal conjunction. In English, they are the same ("The cat **and** the dog played **and** ran"). Some languages use two different words, such as Japanese ("猫**と**犬が遊ん**で**走った").

<div style="text-align:center" TITLE="Hours Needed to Learn Language by Nominal and Verbal Conjunction"><img src="../img/AHQSSRADDL/nominal_verbal_conjunction.png" /></div>

<!-- 
ax = langDf.pivot_table(values=['class_hours'], index='64A Nominal and Verbal Conjunction').sort_values('class_hours').plot(kind='bar', color='#8ED08B');

fontColor = "#a61717"

plt.autoscale(tight=True)

ax.set_frame_on(False)

ax.set_xticklabels(["Identified", "Differentiated"], color=fontColor, rotation=0)
ax.set_yticklabels(["0", "200", "400", "600", "800", "1000", "1200", "1400", "1600"], color=fontColor)

plt.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom=False,      # ticks along the bottom edge are off
    top=False,         # ticks along the top edge are off
    left=False,         # ticks along the top edge are off
    labelbottom=True) # labels along the bottom edge are off

ax.set_xlabel('')

plt.title("Hours Needed to Learn Language by Nominal and Verbal Conjunction", color=fontColor);
ax.get_legend().remove()
plt.show()

-->

Languages with differentiated conjunction tend to be much more difficult, though I doubt this alone would contribute hugely to language difficulty.

Something similar can be observed with the comitative and instrumental cases. Comitative case occurs when something is accompanied by something else (I went **with** him). The instrumental case occurs when something is used as an instrument (I cut **with** a knife). Some languages differentiate between the two with different markings, while English does not. For example, in Japanese ("彼と**一緒**に行きました", "ナイフ**で**切る"). 

<div style="text-align:center" TITLE="Hours Needed to Learn Language by Comitatives and Instrumentals"><img src="../img/AHQSSRADDL/comitative_instrumental.png" /></div>

<!-- 
ax = langDf.pivot_table(values=['class_hours'], index='52A Comitatives and Instrumentals').sort_values('class_hours').plot(kind='bar', color='#8ED08B');

fontColor = "#a61717"

plt.autoscale(tight=True)

ax.set_frame_on(False)

ax.set_xticklabels(["Identified", "Mixed", "Differentiated"], color=fontColor, rotation=0)
ax.set_yticklabels(["0", "200", "400", "600", "800", "1000", "1200"], color=fontColor)

plt.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom=False,      # ticks along the bottom edge are off
    top=False,         # ticks along the top edge are off
    left=False,         # ticks along the top edge are off
    labelbottom=True) # labels along the bottom edge are off

ax.set_xlabel('')

plt.title("Hours Needed to Learn Language by Comitatives and Instrumentals", color=fontColor);
ax.get_legend().remove()
plt.show()
-->

While I'd expect this to matter, again, I doubt this alone would contribute hugely to language difficulty.

Another significant result can be seen with suppletion. This is a phenomenon whereby two paradigms within a language creates irregularities within the language on their interface. The most accessible example is "go" vs. "went". Both go and the, now-archaic, "wende" (meaning the same thing as go) ended up occupying the same semantic territory. Nowadays, most of "wende"'s cognates are gone, but "went" remains. It's common for such irregularities to form across tense and aspect lines.

<div style="text-align:center" TITLE="Hours Needed to Learn Language by Suppletion According to Tense and Aspect"><img src="../img/AHQSSRADDL/suppletion.png" /></div>

<!-- 
ax = langDf.pivot_table(values=['class_hours'], index='79A Suppletion According to Tense and Aspect').sort_values('class_hours').plot(kind='bar', color='#8ED08B');

fontColor = "#a61717"

plt.autoscale(tight=True)

ax.set_frame_on(False)

ax.set_xticklabels(["Tense", "Tense and Aspect", "None"], color=fontColor, rotation=0)
ax.set_yticklabels(["0", "200", "400", "600", "800", "1000", "1200", "1400"], color=fontColor)

plt.tick_params(
    axis='both',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom=False,      # ticks along the bottom edge are off
    top=False,         # ticks along the top edge are off
    left=False,         # ticks along the top edge are off
    labelbottom=True) # labels along the bottom edge are off

ax.set_xlabel('')

plt.title("Hours Needed to Learn Language by\nSuppletion According to Tense and Aspect", color=fontColor);
ax.get_legend().remove()
plt.show()

-->

This seems especially suspect to me. Wouldn't a lack of irregularities make a language easier to learn? Perhaps it does, but those languages which lack irregularities may be unlike English, making them overall harder to learn, regardless. Doing a χ² test on this vs genus, one sees that, indeed, the existence of suppletion is tied to a language's history.

    Chi-squared:  67.30446623093682 
    p-value:  0.034314515156764806

I mean, duh, but this confirms that genetically related languages have similar suppletions. This would mean that the genetically most similar languages to English should have similar suppletions to English.

Similar things could be said about the various other significant attributes I've highlighted...

<a name="heading5"></a>
## Goodbye!

I suppose there should be a narrative somewhere here, but there isn't. These languages were not made by thinking gods, and though they manifest through the actions of man, they do not exist by the designs of man. Each is an egregore and such inscrutable entities disdain narration. The best one can hope is that some patterns become clear.

Well, we certainly learned a lot today. Nothing that was definitely true, but, a lot, regardless.

... Bye!
