# Species habitat maps and gap analysis

A workflow to clean species distribution models by elevation and potential habitats and perform a gap analysis

## Data

There are the minimum data requirements to run the workflow:

1. Species potential distribution maps (usually expert based)
2. Species known elevational range
3. Species presence points (e.g. ebird)
4. A list of potential habitats of occurrence for each species 
5. A layer of protected areas intersecting the species distribution map (for gap analysis)

## Codes

### clean_distribution_maps_elev

Filter species distribution polygon by elevation and potential habitat

### generate_habitat_maps

Filter species distribution maps based on potential habitat

### validate_maps_omission

Apply process to validate maps based on presence data

### create_biplot_function

A function to generate biplot maps

### calculate_richness_map

A workflow to create species richness maps for selected groups of species

### biplot_birds_maps

A workflow to generate a biplot based on the biplot function

### Gap_analysis

Workflow to generate a gap analysis based on the percentage of a species distribution map covered by a protected area

# Authors and contributors

Andres Felipe Suarez Castro
