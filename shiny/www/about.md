# **The Jameel Institute Crisis Vaccination Planner (JICVP)**
  
**JICVP**  is an interactive modelling tool designed to forecast outbreak risks for key vaccine-preventable diseases (VPDs) and inform the prioritization of reactive vaccination campaigns in humanitarian emergencies.

In conflict-affected settings, disruptions to routine healthcare and immunization services increase the risk of VPD outbreaks. JICVP addresses this challenge by integrating a stochastic, mathematical model of disease transmission with:
  
* Pre-conflict demographic, immunization and case data
* Flexible impacts on health system functioning and outbreak occurrence
* The ability to model a variety of settings and diseases

The tool generates time-dependent outbreak probabilities and estimated case burdens, allowing users to:
  
* Simulate how vaccination disruptions increase susceptibility over time
* Quantify outbreak risks under varying vaccination coverage scenarios
* Assess outbreak severity and the impact of delayed or proactive vaccination campaigns

By enabling users to explore different intervention scenarios, JICVP supports rapid decision-making to mitigate the public health impacts of immunization disruptions.

## **The Underlying Model**

The JICVP model builds on a generalisable, stochastic SEIR (Susceptible–Exposed–Infectious–Recovered) framework, extended to include compartments for severe infection (**Is**) and recovery with complications (**Rc**). The model is stratified by age, vaccination status, and risk group, and incorporates dynamic processes such as aging, vaccination, waning immunity, and movement between risk strata.

Epidemiological dynamics are informed by a combination of empirical and synthetic data, including:
  
- [UN World Population Prospects](https://population.un.org/wpp/) for demographic structure, fertility, mortality and migration
- [WHO](https://immunizationdata.who.int/) for historical vaccine coverage and case data
- [POLYMOD study](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697) for age-specific contact matrices
- Disease-specific parameters including R₀, incubation and infectious periods, and vaccine efficacy

To estimate prior exposure and immunity, the model supports several approaches: constant seeding of infections, direct use of WHO-reported case data, or hybrid methods that combine empirical and simulated data depending on disease and country context.

## **Intended Use**

JICVP is intended as a **decision-support tool** for planning vaccination responses in crisis settings. It does **not provide real-time outbreak forecasts** or directly predict current epidemiological outcomes.

Instead, it helps users:
  
- Understand **outbreak risks** under different disruption scenarios
- Estimate the **value of restoring vaccination**
- Support **strategic decision-making** around timing, targeting, and disease prioritisation in resource-limited settings

JICVP is currently in a prototype phase and is not intended for real-time outbreak prediction. Instead, it serves as a planning tool to illustrate the potential consequences of immunization disruptions and the benefits of various vaccination strategies in crisis settings.

We are actively engaging with stakeholders, including humanitarian partners, to guide the future development of JICVP and ensure it aligns with operational needs in the field.

## **Contact us**

For inquiries or to provide feedback on JICVP, please contact: oliver.watson15@imperial.ac.uk

## **Contribution Acknowledgements**

JICVP is developed by the Jameel Institute at Imperial College London, with contributions from:
  
* Arran Hamlet<sup>1,2</sup>
* Paula Christen<sup>1,2,3</sup>
* James Hay<sup>4,5</sup>
* Bhargavi Rao<sup>6,7</sup>
* Oliver Watson<sup>1,2</sup>
  
## **Affiliations**
  
1) Jameel Institute, School of Public Health, Imperial College London, London, United Kingdom.
2) MRC Centre for Global Infectious Disease Analysis, School of Public Health, Imperial College London, London, UK.
3) Center for Epidemiological Modelling and Analysis (CEMA), University of Nairobi, Nairobi, Kenya.
4) Big Data Institute, Li Ka Shing Centre for Health Information and Discovery, Nuffield Department of Medicine, University of Oxford, Oxford, UK.
5) Pandemic Sciences Institute, Nuffield Department of Medicine, University of Oxford, Oxford, UK.
6) Médecins Sans Frontières, London, UK.
7) London School of Hygiene and Tropical Medicine, London, UK.

## **Funding Acknowledgements**

This project is supported by [Community Jameel](https://www.communityjameel.org/) as part of the Jameel Institute – Realtime Intelligent Support for Emergencies (JI-RISE) initiative

<img src="www/jameel-community-logo.png">

# **Disclaimer and Licensing**

## **Model code license**

The JICVP model code is provided “as is”, without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose and noninfringement.

In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software.

---
  
  