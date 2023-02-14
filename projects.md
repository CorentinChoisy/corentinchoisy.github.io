---
layout: page
title: Projects
permalink: projects/
---

Things I do, including research, academic course projects, and miscellaneous interests.

### Research
<ul>
    {% for pub in site.categories.papers %}
    {% assign title = pub.id | split: "/" %}
    <li><a href="{{ pub.url }}" style="text-transform: capitalize">{{ title[2] }}</a> <small style="color: #c0c0c0">{{ pub.year }}</small></li>
    {% endfor %}
</ul>

### 4th-year Master's Thesis
[Multiple Testing Correction in Epidemiology][trefoil] <small style="color: #c0c0c0">2015</small>


### Other
<ul>
<li><a href="https://github.com/corentinchoisy/corentinchoisy.github.io"><code>corentinchoisy.github.io</code> on Github</a></li>
<li><a href="{{ site.url }}/projects/ur1down">UR1 RMarkdown templace</a> <small style="color: #c0c0c0">2021</small></li>
<li><a href="{{ site.url }}/projects/star-helix">Star Helix Games, roleplaying game design</a> <small style="color: #c0c0c0">2021</small></li>
</ul>

[trefoil]: {{ site.url }}/projects/memoire "4th year narrative review project in collaboration with two other students."
