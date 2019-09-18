# Results for Afghanistan 2019 Presidential Elections

This repository will host (if and when elections are held) results data and other information for the 2019 Afghan presidential election, as  reported by the [Independent Election Commission of Afghanistan (IEC)](http://www.iec.org.af/). In the interim, the repository hosts historical data from past presidential elections, and administrative data (primarily polling center plans and voter registration data) released by the IEC in advance of the 2019 elections, which (after several previous postponements) are currently scheduled to take place on September 28 2019.

This data is being released in open source for the purposes of contributing to public understanding of the elections and to allow for the analysis of available IEC reporting. Please read all accompanying documentation and link, cite, and credit as appropriate. 

## Table of contents
- **[Summary of files](https://github.com/colincookman/afghanistan_presidential_election_2019#summary-of-files)**
- **[Summary of results data](https://github.com/colincookman/afghanistan_presidential_election_2019#summary-of-results-data)**
- **[Background to the election](https://github.com/colincookman/afghanistan_presidential_election_2019#background-to-the-election)**
- **[Primary sources for this dataset](https://github.com/colincookman/afghanistan_presidential_election_2019#primary-sources-for-this-dataset)**
- **[Caveats, gaps, and errors](https://github.com/colincookman/afghanistan_presidential_election_2019#caveats-gaps-and-errors)**
- **[Contact and acknowledgements](https://github.com/colincookman/afghanistan_presidential_election_2019#contact-and-acknowledgements)**

## Summary of files

Once results are finalized, the underlying R code used to scrape, clean, and reorganize all data will also be published to allow for replication of results, although no guarantees are made as to its long-term functionality.

The main keyfiles collecting the metadata used to aggregate, disaggregate, and join the results datasets are:
- **[candidate_key.csv](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/candidate_key_2019.csv)** for a list of all registered candidates. Includes unique IEC-assigned ID codes, names in Dari (official IEC transliterations into English are pending until the results release), incumbency status, and gender.
- **[ps_key.csv](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/ps_key_2019.csv)** for a list of all unique polling station codes, their corresponding parent polling center codes, designation for use by either male or female voters, and total number of assigned registered voters.
- **[pc_key.csv](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/pc_key_2019.csv)** for a list of all planned (and non-planned) polling centers, their respective polling station counts, total voter registration data, and the center's reporting status in the 2018 parliamentary elections.
- **[district_key.csv](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/district_key_2019.csv)** for a list of all unique districts as reported by the IEC (as well as the administrative subdivisions of Kabul, Jalalabad, Mazar-e-Sharif, Kandahar, and Herat); see also the **[district_code_keyfile](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/district_data/district_code_keyfile_2019.csv)** for a key used to track and match district codes across election cycles and datasets, drawing on the work of the [Afghanistan District Maps project](https://mapsynch.maps.arcgis.com/apps/MapSeries/index.html?appid=fe0f16a7b8da4157a7d7f9451a802d74#);
- **[province_key.csv](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/province_key_2019.csv)** for a list of provinces, their codes, and names in English, Dari, and Pashto.

## Summary of results data

Once elections are held and results released by the IEC, detailed results for all candidates across all reporting polling stations will be cleaned, restructured, and published in this repository in .csv file format. Results will also be re-aggregated to the polling center, district, and provincial level. If the IEC follows past precedent, both preliminary and final certified results data should be available at all levels.

## Background to the election

Afghanistan has conducted three nationwide presidential elections since the adoption of the 2004 Constitution; the 2014 election was the first in which President Hamid Karzai, constrained by term limits, did not return as an incumbent. After no candidate met the fifty percent majority requirement in the first round of that election, held in April 2014, former finance minister Ashraf Ghani and former foreign minister Abdullah Abdullah (previously the runner-up against Karzai in the 2009 elections) competed in a second round runoff election in June 2014. Abdullah and his supporters [disputed the outcome](https://www.washingtonpost.com/world/asia_pacific/abdullah-mobilizes-supporters-vows-to-challenge-afghan-election-results/2014/07/08/3fd4bd0c-0690-11e4-8a6a-19355c7e870a_story.html) of the second round, alleging fraud, and forced an UN-monitored audit of the results. The dispute was ultimately resolved through direct mediation by then-Secretary of State John Kerry that led to a [power-sharing agreement](https://www.afghanistan-analysts.org/miscellaneous/aan-resources/the-government-of-national-unity-deal-full-text/) between Ghani as president and Abdullah in a new "chief executive" position as part of a newly-formed 'National Unity Government'. 

The unity government agreement included commitments to a process of electoral reform, which [proceeded fitfully](https://muse.jhu.edu/article/702129) over the course of the government's first years in office, as multiple committees were formed and re-constituted to consider reform proposals. In September 2016 President Ghani [issued a new electoral law](https://www.afghanistan-analysts.org/afghanistans-incomplete-new-electoral-law-changes-and-controversies/) by executive decree, overriding parliamentary objections, and in November 2016 new commissioners were appointed to the IEC and the Electoral Complaints Commission (ECC), which is responsible for adjudicating election complaints. The chairman of the IEC [was again replaced](https://www.reuters.com/article/us-afghanistan-election/head-of-afghan-election-body-sacked-raising-doubts-over-2018-ballots-idUSKBN1DF2R3) a year later in November 2017. Amidst the ongoing electoral reform debates and IEC shakeups, parliamentary elections [were indefinitely postponed](https://www.nytimes.com/2015/06/20/world/asia/afghan-parliaments-term-is-extended-after-squabbles-delay-elections.html) past the five-year expiration of parliament's term in 2015, and after multiple delays were finally held in October 2018; see [separate repository](https://github.com/colincookman/afghanistan_election_results_2018) for results and analysis of those elections. 

On December 30 2018, the IEC leadership [announced the postponement](https://www.washingtonpost.com/world/asia_pacific/afghanistans-presidential-elections-delayed-until-july/2018/12/30/038faea0-0c45-11e9-8f0c-6f878a26288a_story.html) of presidential elections from their original date of April 20 2019, to a new date of July 20 2019. In part in response to allegations of fraud and mismanagement of the parliamentary elections — the results of which took six months to finalize — on February 13 2019, President Ghani issued an executive order [modifying Afghanistan's electoral law again and firing all members of the IEC and ECC](https://www.afghanistan-analysts.org/afghanistans-2019-elections-3-new-electoral-commissioners-amendments-to-the-electoral-law/). The eighteen already-registered presidential candidates nominated a pool of potential replacements and cast votes to elect the new commissioners; Ghani held final authority to decide whether an elected commissioner would be appointed to the IEC or ECC, however.

On March 20 2019, the presidential elections [were further postponed](https://www.reuters.com/article/us-afghanistan-election/afghanistan-presidential-election-postponed-to-september-idUSKCN1R11X1) by the new IEC leadership to a new date, September 28 2019. Elections for provincial councils, district councils (which have never been elected, despite being mandated under the 2004 Constitution), and parliamentary elections for Ghazni province (which were not held in 2018) were to have taken place simultaneous to the presidential election; however, on May 29 2019 IEC chairwoman Hawa Alam Nuristani [announced that](https://www.rferl.org/a/afghanistan-postpones-two-local-elections/29970772.html) plans for those elections would be dropped, and did not specify a new date on which they might be held.

President Ghani's constitutional tenure expired on May 21 2019, but he remains in office (over the [objection of his rivals](https://www.tolonews.com/afghanistan/candidates-warn-civil-disobedience-against-govt)) until elections are held, with his extension [endorsed by a Supreme Court ruling](https://www.tolonews.com/afghanistan/supreme-court-extends-president-ghani%E2%80%99s-tenure) in April 2019. On August 6 2019, former foreign minister Zalmay Rasool [withdrew from the race](https://www.khaama.com/zalmai-rassoul-withdraws-from-september-presidential-race-03572/) and endorsed Ghani. Following disputes within his coalition of supporters, former national security advisor Hanif Atmar [suspended](https://www.tolonews.com/index.php/afghanistan/atmars-team-suspends-election-campaign) his campaign on August 8 2019; one of his running mates, Mohammad Mohaqiq, subsequently [shifted his support](http://reporterly.net/latest-stories/priority-to-peace-not-election-led-mohaqiq-to-leave-hanif-atmars-election-team/) to Abdullah Abdullah, but as of this writing Atmar has not endorsed another campaign. 

Negotiations between U.S. diplomats and Taliban representatives in Qatar, and the possibility of a new power-sharing negotiation between Afghan factions as an outcome of those talks, [have overshadowed preparations for the elections](https://www.nytimes.com/2019/08/29/world/asia/afghanistan-election-taliban.html), and public campaign activities by most of the candidates [have been limited](https://www.afghanistan-analysts.org/afghanistans-2019-election-7-dithering-over-peace-amid-a-lacklustre-campaign/) for much of the official campaign period. President Ghani has insisted that the elections will proceed as scheduled and has rejected alternative proposals for an interim government proposed by some of his rivals. The IEC [began the distribution](https://ariananews.af/iec-dispatches-biometric-devices-to-10-provinces/) of election materials, including biometric voter verification devices, on August 30 2019, and announced that it had [completed delivery of those materials](https://www.tolonews.com/afghanistan/iec-wraps-delivery-election-materials) in 33 out of 34 provinces as of September 16 2009.

## Primary sources for this dataset

### Candidate data
Candidate registration began on December 22 2018, and closed in early February 2019. On April 26 2019, the IEC [assigned](http://www.iec.org.af/pdf/cn-1398/final-presidential-candidateslist.pdf) ballot positions, ballot symbols, and candidate ID codes to the eighteen registered presidential candidates, accepting all candidates who filed nominations to contest the elections. No candidates were assigned formal party affiliations on the ballot. (The final candidate list published by the IEC did not include transliterations of candidate names; the [candidate key](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/candidate_key_2019.csv) will be updated to reflect the official IEC choice of transliteration once English-language results are published.) 

The [preliminary list of registered candidates](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/raw/candidate_data/preliminary-presidential-candidates-list.pdf) also includes vice presidential running mates; see also [profiles of the tickets by the Afghan Analysts Network](https://www.afghanistan-analysts.org/afghanistans-2019-elections-2-who-are-running-to-become-the-next-president/). Despite their withdrawals from the race, both Zalmay Rasool and Hanif Atmar will remain as options on the final ballots.

### Polling center plans
The IEC's preparations for the 2019 elections are, with some exceptions, based on the same universe of polling centers generated for the 2018 parliamentary elections. Out of an initial universe of 7366 polling centers that appeared on its website as of early June, in early August the IEC filtered that list to display only 5707 polling centers, without providing explanation for the change.

On August 19 2019, the IEC [published a final polling center plan](http://www.iec.org.af/pdf/Elections2019PollingStations.pdf), which included details, including polling station counts and voter registration data, for 5373 polling centers. At a press conference on August 28 2019, Interior Minister Massoud Andarabi [said that](http://reporterly.net/live/newsfeed/wednesday-august-28/431-polling-centers-to-be-closed-on-election-day-moi/) another 431 polling centers would be closed due to security threats, but did not specify which centers would be affected. Excluding centers in Ghazni province, where no parliamentary elections were held in 2018, 49 of the plann 2019 centers were not previously planned to open in 2018. 453 were planned to open but were not included (either due to disqualification or failure to report results, IEC reporting has not clarified either way) in the 2018 preliminary results, and 506 were not included in final results.

The IEC's polling center plan, not accounting for those further security closures, includes plans for 29,586 polling stations — 18,467 assigned for use by male voters, and 11,119 assigned for female voters. (Each polling station represents a booth within the polling center, from a minimum of one station per center to a maximum of 36.) The IEC did not publish a pre-election polling station plan for the 2018 parliamentary elections that classified stations for male or female voters, which would allow for direct comparison, but in those elections reported preliminary results from 17,546 polling stations, and final results from 17,713 stations. The increase in the number of planned stations appears in part due to a decision by the IEC for 2019 to lower the maximum ballot allocation / registration per station to 400. In Ghazni, 828 polling stations are planned that would have been ommited from 2018 plans.

### Voter registration data
Afghanistan introduced polling center-based voter registration lists for the first time in the 2018 parliamentary elections, and that data provides a baseline for 2019 voter registration. In June 2019, the IEC conducted an approximately monthlong ["top-up" registration exercise](http://www.iec.org.af/en/voters/voter-list) to register newly eligible voters or otherwise update voter rolls; registration changes were only possible to be made at a limited number of polling centers, listed by the IEC [here](http://www.iec.org.af/pdf/VRtopuppc.pdf).

On the week of July 1 2019, the IEC announced preliminary figures from this top-up exercise, and published [provincial-level reports](http://www.iec.org.af/en/voters/vr-statistics) for a total of 5056 polling centers (identified by name only and not polling center code). Ghazni province was ommitted from these provincial reports. The preliminary data ommited 15 polling centers with previously known voter registration data, and added registration data for five polling centers that had no previously recorded registration data (or votes) in 2018.

In an [accompanying statement](http://www.iec.org.af/en/media-gallery/press-releases/581-press-end-of-vr-20190701) to the July update, the IEC stated that it had added a net increase of 317,395 new voters through the top-up exercise, although it emphasized that the data released at this time was not final. It also said that registration would continue past the one month top-up period in Ghazni. The actual net change from the detailed preliminary provincial reports released alongside this statement was a *decline* of -8,443 registered voters from 2018, however; the reported preliminary 2019 registration (minus Ghazni) totaled 8.834 million, compared to 8.843 million total known registered voters in 2018. In subsequent public comments, IEC officials were quoted saying that [another approximately 400,000 "ghost voters" or duplicate registrations had been removed from the rolls](http://www.iec.org.af/en/media-gallery/press-releases/581-press-end-of-vr-20190701), but no explanation was offered for the earlier discrepancies or details provided on these further adjustments.

As detailed in the [polling center key](https://github.com/colincookman/afghanistan_election_results_2018/blob/master/keyfiles/pc_key_2019.csv), the August 19 publication of the IEC's pre-election polling center plan included final voter registration data for all planned centers, superseding the earlier preliminary registration data. These new final registration figures for 2019 total 9.665 million voters for all planned polling centers. Excluding Ghazni, for which there was no previous 2018 registration data, this represents a net increase of approximately +595,000 voters over the earlier preliminary 2019 figures, and approximately +587,000 over 2018 data. (The new total voter registration for Ghazni is 235,213 registered voters.) The IEC has again not offered a public explanation for the discrepanacy between these figures and earlier statements.

The 2019 elections are first in which the IEC has specified polling station-level voter lists; under current planned procedures, voters should undergo a biometric verification process prior to casting their vote at their assigned station. Polling stations that report votes in excess of their registration should be flagged in the results management system and are subject to disqualification. As noted above, the IEC has lowered the maximum number of ballots allocated to a polling station from 600 in previous presidential elections to 400, increasing the number of stations and accompanying staff in some centers. The [polling station key](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/keyfiles/ps_key_2019.csv) includes the number of registered voters assigned to each polling station as reported by the IEC in its pre-election polling station plan.

### Geospatial data
As part of the [2018 parliamentary elections results project](https://github.com/colincookman/afghanistan_election_results_2018), the author received a dataset of latitude / longitude coordinates for 7413 known polling centers from an election observer. Because the 2019 election polling center plan is based on the same list as 2018, this location data also corresponds to the 2019 centers. 

Although polling centers are public sites, these coordinates were shared with the request that it not be published in raw format so as to avoid any potential security risk to the physical polling center locations. Accordingly, absent reporting of this information by another source, the raw coordinate data will not be included in this dataset.

However, the available coordinate data was used to calculate nearest neighbor distances for the five closest polling centers for each respective center (in kilometers using the Vincenty ellipsoid formula, and in lat/lon decimal degrees), which can be found in the 2018 repository's  [pc_plan_nearest_neighbor_coordinates.csv](https://github.com/colincookman/afghanistan_election_results_2018/blob/master/pc_plan/pc_plan_nearest_neighbor_coordinates.csv) file and may be used for further geospatial analysis of results. (See also note below on ongoing efforts to match polling centers to previously published 2014 geocoordinates.)

### Historical election data
This dataset also hosts cleaned and restructured preliminary and final certified results data from Afghanistan's previous three presidential elections held in [2004](https://github.com/colincookman/afghanistan_presidential_election_2019/tree/master/past_elections/presidential_2004), [2009](https://github.com/colincookman/afghanistan_presidential_election_2019/tree/master/past_elections/presidential_2009), and [2014](https://github.com/colincookman/afghanistan_presidential_election_2019/tree/master/past_elections/presidential_2014) (both first round and second round runoff). The detailed polling station-level results files for the 2009 elections are too large to be hosted on Github but are hosted remotely on Google Drive here ([prelim](https://drive.google.com/open?id=1UX_LdOAz6n_EyqIcTWu_x6-VcFeGcSYt) / [final](https://drive.google.com/open?id=1I573Gic0CL6GVN2CStoi-NWS9k1tKBEK)); the ps_lite files in each subfolder includes the basic polling station, candidate code, and vote data.

In addition to past presidential election cycles in this repository, the author's dataset of Afghan parliamentary elections results (for the 2005, 2010, and 2018 parliamentary elections) can be found in a [separate repository](https://github.com/colincookman/afghanistan_election_results_2018).

The historical presidential election results data were drawn wherever available from the original IEC results, as found on its website during the June-September 2019 period (see results sections for [2004](http://www.iec.org.af/public_html/Election%20Results%20Website/english/english.htm), [2009](http://www.iec.org.af/results_2009/), and [2014](http://www.iec.org.af/results_2014/en/elections)). (For the 2004 and 2005 election cycles, elections were administered by the Joint Election Monitoring Body, a joint UN-Afghan government body.) The IEC's results reporting sections offer varying levels of detail or completeness; only provincial-level data is available for the 2004 elections. Some older sections of the IEC website have also been broken by subsequent website redesigns, making it currently impossible to cross-check more detailed polling center-level results on the web against pdf releases. Use of the [Wayback Machine](https://web.archive.org/) is recommended as an additional reference for contemporaneous presentations of the results, dating back to 2008.

Some historical election data was also drawn from or otherwise cross-referenced with the [Afghanistan Election Data project](https://afghanistanelectiondata.org/open/data) (a project of the [National Democratic Institute](https://www.ndi.org/asia/afghanistan) and [Development Seed](https://developmentseed.org/)). (Although note that the AED project only offers preliminary results for the 2009 and 2014 elections.) For this repository, detailed results for the 2014 run-off election were also derived in part from a internal IEC document summarizing the outcome of the 2014 second round audit, which was obtained from an election observer. (As part of the national unity government agreement, the final results of the 2014 elections were initially withheld from publication, but [were later released by the IEC](https://www.reuters.com/article/us-afghanistan-election/commission-releases-disputed-2014-afghan-election-results-idUSKCN0VX1O8) in February 2016.)

Caution is advised that polling center and polling station codes *do not correspond* across election periods (nor do provincial or district codes - see the [district_code_keyfile](https://github.com/colincookman/afghanistan_presidential_election_2019/blob/master/district_data/district_code_keyfile_2019.csv) for a join key at those levels). The AED project includes latitude and longitude coordinates for 2014 polling centers, derived from a now-inoperative IEC ArcGIS site; that list includes some polling centers that do not appear in other IEC publications, however, and the author is currently unable to verify their completeness or accuracy. In at least some cases, geocoordinates for known polling center locations that are not believed to have been relocated by the IEC do not appear to match between the 2014 and 2018 coordinate sets; analysis is ongoing in an effort to more conclusively match at the center level between the 2014 and 2018 elections.

The underlying R code used to clean up the available historical results data, and the raw IEC files used for this purpose, are included their respective subfolders. Some additional manual cleaning of older data was also undertaken that may not be fully reflected in this code, and caution in advised in running it without careful checks on the process.

### Other data

For mapping purposes, UN OCHA has published a set of shapefiles for district boundaries, which can be found on the [Humanitarian Data Exchange](https://data.humdata.org/dataset/afg-admin-boundaries) and is also included in the [district data subfolder](https://github.com/colincookman/afghanistan_election_results_2018/tree/master/district_data/administrative_boundaries) of the 2018 parliamentary elections project for reference. Further transformations are required to re-base 2018-19 districts or election results data to match these district boundaries, however. For an unofficial, ArcGIS-only version of the 421 districts used in the most recent IEC data, see [this Map Sync map](https://mapsynch.maps.arcgis.com/home/item.html?id=b660fd5ee9c744bfaebff04c8cf15aab).

## Caveats, gaps, and errors
This repository remains a work in progress and caution is advised when using it for analysis. The author cannot verify the accuracy of, or account for any discrepancies in, the underlying data, and makes no guarantees as to its completeness. The results collected may be further altered by legal complaints, IEC amendments to the results, or other disputes and adjudication processes.

## Contact and acknowledgements
Feedback, corrections, or suggestions for further expansion or collaboration are greatly appreciated. For questions, suggestions, or to contribute further, please leave an issue request here on Github or contact [Colin Cookman](https://colincookman.wordpress.com/about/) by email or Twitter.

Although I am solely responsible for any errors in this project, I am grateful to Lucy Stevenson-Yang for research and coding assistance; to the Independent Election Commission for releasing elections data in a public form that allows for open analysis; to the anonymous election observer sources who generously provided additional data sources; to the [Afghanistan Elections Data](https://afghanistanelectiondata.org/front), [Map Sync](https://mapsynch.maps.arcgis.com/apps/MapSeries/index.html?appid=fe0f16a7b8da4157a7d7f9451a802d74), and [OCHA Afghanistan / Humanitarian Data Exchange](https://data.humdata.org/organization/ocha-afghanistan) projects for their work on collecting other supplementary data; to the researchers and authors at the [Afghan Analysts Network](https://www.afghanistan-analysts.org/) for their detailed reporting and analytical resources which have informed my understanding of Afghanistan's elections; to John Ray, Luke Sonnet, and the Stackoverflow and RStudio communities for lessons in the use of R for data collection, cleaning, and analysis; to [Democracy International](http://democracyinternational.com/) for providing the opportunity for me to gain firsthand experience as an observer to the 2010 and 2014 Afghan elections; and to Belquis Ahmadi, Staffan Darnolf, Scott Smith, Andrew Wilder, Scott Worden and many other colleagues at the [U.S. Institute of Peace](https://www.usip.org/regions/asia/afghanistan) for their guidance, support, mentorship, and patience as I worked on developing this project.