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

### Academic
<ul>
    <li><a href="{{ site.url }}/projects/cs-6750-health-easel">Health Easel</a> <small style="color: #c0c0c0">2017</small></li>
    <li><a href="{{ site.url }}/projects/cs-7450-a-viz-of-ice-and-fire">A Viz of Ice and Fire</a> <small style="color: #c0c0c0">2016</small></li>
    <li><a href="{{ site.url }}/projects/materials-informatics-grain-growth">Materials Informatics: Grain Growth</a> <small style="color: #c0c0c0">2016</small></li>
    <li><a href="{{ site.url }}/projects/cse-6730-bobby-dodd-simulation">Modeling of Pedestrian Traffic Around Bobby-Dodd Stadium</a> <small style="color: #c0c0c0">2016</small></li>
    <li><a href="{{ site.url }}/projects/uga-undergrad-course-projects">UGA Undergrad Course Projects</a></li>
    <ul style="padding-left: 3rem;">
        <li>Image Compression <small style="color: #c0c0c0">2014</small></li>
        <li>Railgun Simulation <small style="color: #c0c0c0">2014</small></li>
        <li>Path Minimization <small style="color: #c0c0c0">2013</small></li>
        <li>Numerical ODE Solution and Integration Project <small style="color: #c0c0c0">2013</small></li>
    </ul>
    <li><a href="{{ site.url }}/projects/cube-decomposition-trophy">Cube Decomposition Trophy</a> <small style="color: #c0c0c0">2014</small></li>
    <li><a href="{{ site.url }}/projects/uga-keychain">UGA Keychain</a> <small style="color: #c0c0c0">2014</small></li>
</ul>

### Other
<ul>
<li><a href="https://github.com/corentinchoisy/corentinchoisy.github.io"><code>corentinchoisy.github.io</code> on Github</a></li>
<li><a href="{{ site.url }}/projects/ur1down">UR1 RMarkdown templace</a> <small style="color: #c0c0c0">2021</small></li>
<li><a href="{{ site.url }}/projects/star-helix">Star Helix Games, roleplaying game design</a> <small style="color: #c0c0c0">2021</small></li>
</ul>

[trefoil]: {{ site.url }}/projects/memoire "4th year narrative review project in collaboration with two other students."
