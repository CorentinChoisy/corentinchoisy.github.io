---
layout: cv
title: CV
permalink: cv/
jsarr:
- js/scripts.js
---

<h1 id="cv-title"><a href="{{ site.url }}">Corentin Choisy</a></h1>

<p id="cv-subtitle"><i>Epidemiology Student (<span class="cv-vis">Modeling</span> + <span class="cv-ai">Methodology</span>)</i></p>

<!-- <div id="cv-toc">
<ul class="cv-description">
	<li>Education</li>
	<li>Industry Research</li>
	<li>Academic Research</li>
	<li>Honors and Awards</li>
	<li>Publications</li>
	<li>Talks</li>
	<li>Press</li>
	<li>Teaching</li>
	<li>Mentoring</li>
	<li>Grants and Funding</li>
	<li>Interactive Articles</li>
	<li>Service</li>
	<li>Design</li>
	<li>References</li>
</ul>
</div> -->

<div>
I build <b><span class="cv-vis">epidemiologic and etiologic models</span></b> and provide a bigger <b><span class="cv-ai">methodological</span></b> sensitivity to epidemiologic and clinical studies. I'm also planning to focus more on longitudinal and survival models.
</div>

<div class="cv-spacer"></div>

<div>
I've benefited from teachings and will work as an intern with multiple researchers and engineers from multiple public research labs in France. I'm planning on starting a PhD project in the next few years.
</div>

<!-- <div class="cv-spacer"></div>

<div>
My research is supported by a NASA Space Technology Research Fellowship.
</div> -->

<div class="cv-spacer"></div>

<div class="cv-image-links-wrapper">
	<div class="cv-image-links">
		{% for link in site.data.social-links %}
			{% if link.cv-group == 1 %}
				{% include social-link.html link=link %}
			{% endif %}
		{% endfor %}
	</div>
	<div class="cv-image-links">
		{% for link in site.data.social-links %}
			{% if link.cv-group == 2 %}
				{% include social-link.html link=link %}
			{% endif %}
		{% endfor %}
	</div>
</div>

***

## Education

{::nomarkdown}
{% for degree in site.data.education %}
{% include cv/degree.html degree=degree %}
{% endfor %}
{:/}

## Academic Research Experience

{% for experience in site.data.experiences %}
{% if experience.type == 'academic' %}
{% include cv/experience.html experience=experience %}
{% endif %}
{% endfor %}


## Publications and Projects

### Latest

{% assign selectedBoolForBibtex = true %}

{% assign selected = site.categories.papers | where: 'selected', true %}
{% for pub in selected %}
{% include cv/publication.html pub=pub %}
{% endfor %}

<!-- ### All Publications -->

{% assign selectedBoolForBibtex = false %}


>### Journal

>{% assign journal = site.categories.papers | where: 'type', "journal" %}
{% for pub in journal %}
{% include cv/publication.html pub=pub selectedBoolForBibtex=selectedBoolForBibtex %}
{% endfor %}

>### Conference

>{% assign conference = site.categories.papers | where: 'type', "conference" %}
{% for pub in conference %}
{% include cv/publication.html pub=pub selectedBoolForBibtex=selectedBoolForBibtex %}
{% endfor %}

>### Miscellaneous

>{% assign preprint = site.categories.papers | where: 'type', "misc" %}
{% for pub in preprint %}
{% include cv/publication.html pub=pub selectedBoolForBibtex=selectedBoolForBibtex %}
{% endfor %}

## Teaching

{% for teach in site.data.teaching %}
{% include cv/teaching.html teach=teach %}
{% endfor %}

## Technology Skills

{% for skill in site.data.skills %}
{% include cv/skill.html skill=skill %}
{% endfor %}

>## Service

><div class="cv-service-title"><b>Organizer</b></div>
{% for venue in site.data.organizer %}
{% include cv/venue.html venue=venue %}
{% endfor %}

><div class="cv-service-title"><b>Program Commitee</b></div>
{% for venue in site.data.pc %}
{% include cv/venue.html venue=venue %}
{% endfor %}

><div class="cv-service-title"><b>Reviewer</b></div>
{% for venue in site.data.reviewer %}
{% include cv/venue.html venue=venue %}
{% endfor %}

><div class="cv-service-title"><b>Institutional</b></div>
{% for institution in site.data.institutional %}
{% include cv/institutional.html institution=institution %}
{% endfor %}

><div class="cv-service-title"><b>Member</b></div>
{% for member in site.data.memberships %}
{% include cv/member.html member=member %}
{% endfor %}

## References

{% for reference in site.data.references %}
{% include cv/reference.html reference=reference %}
{% endfor %}

## Contact

Corentin Choisy  
`corentin.choisy@etudiant.univ-rennes1.fr`  
220, 2 rue d'Alsace  
35000 Rennes
<span style="background: linear-gradient(0deg, #34495e, #3498db); -webkit-background-clip: text; -webkit-text-fill-color: transparent; display: block">
—  
Bretagne
France
</span>


[cv]: {{ site.url }}/cv.pdf "My CV."

<!--[poloclub]: http://poloclub.gatech.edu "Polo Club of Data Science"
[gt]: http://gatech.edu "Georgia Tech"
[cse]: http://cse.gatech.edu "GT Computational Science and Engineering"
[coc]: http://www.cc.gatech.edu "GT College of Computing" -->

[fred]: http://corentinchoisy.github.io "Corentin Choisy"
<!--[polo]: http://www.cc.gatech.edu/~dchau/ "Polo Chau"
[alex]: http://va.gatech.edu/endert/ "Alex Endert" -->

<!--[jpl]: https://www.jpl.nasa.gov/ "NASA Jet Propulsion Lab"
[hi]: https://www.hi.jpl.nasa.gov/ "Human Interfaces Group at NASA JPL"
[pnnl]: https://www.pnnl.gov/ "Pacific Northwest National Laboratory"
[dsa]: http://www.pnnl.gov/nationalsecurity/technical/capabilities/computing/data_sciences.stm "Data Sciences and Analytics Group at PNNL"
[msr]: https://www.microsoft.com/en-us/research/ "Microsoft Research"
[msr-hci]: https://www.microsoft.com/en-us/research/group/human-computer-interaction/ "HCI@MSR" -->

[twitter]: https:/www.twitter.com/corentinchoisy "@corentinchoisy"
[github]: https:/www.github.com/CorentinChoisy "github.com/CorentinChoisy"
<!-- [nstrf]: https://www.nasa.gov/strg/nstrf "NASA Space Technology Research Fellowship" -->
