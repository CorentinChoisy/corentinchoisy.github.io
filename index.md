---
layout: home
title: Home
---

<div id ="intro-wrapper" class="l-middle">
	<div id="intro-title-wrapper" class="intro-left">
		<h1 id="intro-title">Corentin Choisy</h1>
		<div id="intro-subtitle">
			Epidemiology Intern at Inserm UMR 1246 SPHERE
		</div>
	</div>
	<div class="intro-left">
	<div class="intro-left">
		I build epidemiologic and etiologic models and provide a bigger methodological sensitivity to epidemiologic and clinical studies. I'm also planning to focus more on longitudinal and survival models.
	</div>
	<div style="height: 1rem"></div>
	<div class="intro-left">
		I'm currently going through the second year of M.S. in Epidemiology and Clinical Pharmacology Modeling at ' <a href="https://www.univ-rennes1.fr/">Université Rennes 1</a> and have begun a 6-month internship at <a href="https://sphere-inserm.fr/">Inserm UMR 1246 SPHERE</a> in March 2023.
		<!-- enabling machine learning interpretability at scale and for everyone -->
	</div>
	<div style="height: 1rem"></div>
	<div>
		I have benefited from teaching from researchers and engineers at EHESP and IRSET (Rennes, France), Inserm UMR 1246 SPHERE (Nantes, France) and many other French public research labs in epidemiology, clinical research and public health.
	</div>
</div>

<div class="intro-right">
	<img id="intro-image" class="intro-right" src="/images/portrait.jpg">
	<div style="height: 0.5rem"></div>
	<div id="intro-image-links" class="intro-right">
		{% for link in site.data.social-links %}
			{% if link.on-homepage == true %}
				{% include social-link.html link=link %}
			{% endif %}
		{% endfor %}
	</div>
	<div style="height: 0.5rem"></div>
	<div id="intro-cv-wrapper" class="intro-right">
		{% for link in site.data.social-links %}
			{% if link.id == "cv-web" %}
				{% include social-link.html link=link %}
			{% endif %}
		{% endfor %}
	</div>
	</div>
</div>

<h2 class="feature-title l-middle">
	Featured <a href="/dissertation">Papers & Projects</a>
</h2>
<div class="cover-wrapper l-screen">
	{% assign sortedPublications = site.categories.papers | sort: 'feature-order' %}
	{% for feature in sortedPublications %}
		{% if feature.dissertation == true %}
			{% include feature.html feature=feature %}
		{% endif %}
	{% endfor %}
</div>






[gt]: http://www.gatech.edu "Georgia Tech"
[cse]: http://cse.gatech.edu "Georgia Tech Computational Science and Engineering"
[coc]: http://www.cc.gatech.edu "Georgia Tech College of Computing"

[cv]: {{ site.url }}/cv
[polo]: http://www.cc.gatech.edu/~dchau/ "Polo Chau"
[alex]: http://va.gatech.edu/endert/ "Alex Endert"
[poloclub]: http://poloclub.gatech.edu "Polo Club of Data Science"
[nstrf]: https://www.nasa.gov/strg/nstrf "NASA Space Technology Research Fellowship"
