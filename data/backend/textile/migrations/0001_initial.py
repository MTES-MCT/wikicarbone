# Generated by Django 5.0.3 on 2024-03-27 13:47

import django.db.models.deletion
from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = []

    operations = [
        migrations.CreateModel(
            name="Process",
            fields=[
                ("search", models.CharField(blank=True, max_length=200)),
                ("name", models.CharField(max_length=200)),
                ("source", models.CharField(max_length=200)),
                ("info", models.CharField(max_length=200)),
                (
                    "unit",
                    models.CharField(
                        choices=[
                            ("kWh", "kWh"),
                            ("kg", "kg"),
                            ("m2", "m²"),
                            ("MJ", "MJ"),
                            ("t*km", "t*km"),
                        ],
                        max_length=50,
                    ),
                ),
                (
                    "uuid",
                    models.CharField(max_length=50, primary_key=True, serialize=False),
                ),
                ("acd", models.FloatField()),
                ("cch", models.FloatField()),
                ("etf", models.FloatField()),
                ("etfc", models.FloatField()),
                ("fru", models.FloatField()),
                ("fwe", models.FloatField()),
                ("htc", models.FloatField()),
                ("htcc", models.FloatField()),
                ("htn", models.FloatField()),
                ("htnc", models.FloatField()),
                ("ior", models.FloatField()),
                ("ldu", models.FloatField()),
                ("mru", models.FloatField()),
                ("ozd", models.FloatField()),
                ("pco", models.FloatField()),
                ("pma", models.FloatField()),
                ("swe", models.FloatField()),
                ("tre", models.FloatField()),
                ("wtu", models.FloatField()),
                ("pef", models.FloatField()),
                ("ecs", models.FloatField()),
                ("heat_MJ", models.FloatField(default=0)),
                ("elec_pppm", models.FloatField()),
                ("elec_MJ", models.FloatField()),
                ("waste", models.FloatField()),
                ("alias", models.CharField(max_length=50, null=True)),
                (
                    "step_usage",
                    models.CharField(
                        choices=[
                            ("Energie", "Energie"),
                            ("Ennoblissement", "Ennoblissement"),
                            ("Fin de vie", "Fin de vie"),
                            ("Matières, Filature", "Matières, Filature"),
                            ("Tissage / Tricotage", "Tissage / Tricotage"),
                            ("Transport", "Transport"),
                            ("Utilisation", "Utilisation"),
                        ],
                        max_length=50,
                    ),
                ),
                ("correctif", models.CharField(max_length=200)),
            ],
        ),
        migrations.CreateModel(
            name="Material",
            fields=[
                (
                    "id",
                    models.CharField(max_length=50, primary_key=True, serialize=False),
                ),
                ("name", models.CharField(max_length=200)),
                ("shortName", models.CharField(max_length=50)),
                (
                    "origin",
                    models.CharField(
                        choices=[
                            (
                                "ArtificialFromInorganic",
                                "Matière artificielle d'origine inorganique",
                            ),
                            (
                                "ArtificialFromOrganic",
                                "Matière artificielle d'origine organique",
                            ),
                            (
                                "NaturalFromAnimal",
                                "Matière naturelle d'origine animale",
                            ),
                            (
                                "NaturalFromVegetal",
                                "Matière naturelle d'origine végétale",
                            ),
                            ("Synthetic", "Matière synthétique"),
                        ],
                        max_length=50,
                    ),
                ),
                ("geographicOrigin", models.CharField(max_length=200)),
                (
                    "defaultCountry",
                    models.CharField(
                        choices=[
                            ("---", "Inconnu (par défaut)"),
                            ("REO", "Région - Europe de l'Ouest"),
                            ("REE", "Région - Europe de l'Est"),
                            ("RAS", "Région - Asie"),
                            ("RAF", "Région - Afrique"),
                            ("RMO", "Région - Moyen-Orient"),
                            ("RAL", "Région - Amérique Latine"),
                            ("RAN", "Région - Amérique du nord"),
                            ("ROC", "Région - Océanie"),
                            ("AU", "Australie"),
                            ("AL", "Albanie"),
                            ("BD", "Bangladesh"),
                            ("BE", "Belgique"),
                            ("BR", "Brésil"),
                            ("CH", "Suisse"),
                            ("CN", "Chine"),
                            ("CZ", "Tchèquie"),
                            ("DE", "Allemagne"),
                            ("EG", "Egypte"),
                            ("ES", "Espagne"),
                            ("ET", "Ethiopie"),
                            ("FR", "France"),
                            ("GB", "Royaume-Uni"),
                            ("GR", "Grèce"),
                            ("HU", "Hongrie"),
                            ("IN", "Inde"),
                            ("IT", "Italie"),
                            ("KE", "Kenya"),
                            ("KH", "Cambodge"),
                            ("MA", "Maroc"),
                            ("MM", "Myanmar"),
                            ("NL", "Pays-Bas"),
                            ("NZ", "Nouvelle-Zélande"),
                            ("PE", "Pérou"),
                            ("PK", "Pakistan"),
                            ("PL", "Pologne"),
                            ("PT", "Portugal"),
                            ("RO", "Roumanie"),
                            ("LK", "Sri Lanka"),
                            ("TN", "Tunisie"),
                            ("TR", "Turquie"),
                            ("TW", "Taiwan"),
                            ("US", "Etats-Unis"),
                            ("VN", "Vietnam"),
                        ],
                        max_length=3,
                    ),
                ),
                ("priority", models.IntegerField()),
                ("manufacturerAllocation", models.FloatField(blank=True, null=True)),
                ("recycledQualityRatio", models.FloatField(blank=True, null=True)),
                (
                    "recycledFrom",
                    models.ForeignKey(
                        blank=True,
                        null=True,
                        on_delete=django.db.models.deletion.SET_NULL,
                        to="textile.material",
                    ),
                ),
                (
                    "materialProcessUuid",
                    models.ForeignKey(
                        null=True,
                        on_delete=django.db.models.deletion.SET_NULL,
                        related_name="materials",
                        to="textile.process",
                    ),
                ),
                (
                    "recycledProcessUuid",
                    models.ForeignKey(
                        null=True,
                        on_delete=django.db.models.deletion.SET_NULL,
                        related_name="recycledMaterials",
                        to="textile.process",
                    ),
                ),
            ],
        ),
        migrations.CreateModel(
            name="Product",
            fields=[
                (
                    "id",
                    models.CharField(max_length=50, primary_key=True, serialize=False),
                ),
                ("name", models.CharField(max_length=200)),
                ("mass", models.FloatField()),
                ("surfaceMass", models.FloatField()),
                ("yarnSize", models.FloatField()),
                (
                    "fabric",
                    models.CharField(
                        choices=[
                            ("weaving", "Tissage"),
                            ("knitting-straight", "Tricotage Rectiligne"),
                            ("knitting-circular", "Tricotage Circulaire"),
                            ("knitting-integral", "Tricotage Intégral / Whole garment"),
                            (
                                "knitting-fully-fashioned",
                                "Tricotage Fully fashioned / Seamless",
                            ),
                            ("knitting-mix", "Tricotage moyen (par défaut)"),
                        ],
                        max_length=50,
                    ),
                ),
                (
                    "business",
                    models.CharField(
                        choices=[
                            ("small-business", "PME/TPE"),
                            (
                                "large-business-with-services",
                                "Grande entreprise proposant un service de réparation et de garantie",
                            ),
                            (
                                "large-business-without-services",
                                "Grande entreprise ne proposant pas de service de réparation ou de garantie",
                            ),
                        ],
                        max_length=50,
                    ),
                ),
                ("marketingDuration", models.FloatField()),
                ("numberOfReferences", models.IntegerField()),
                ("price", models.FloatField()),
                ("repairCost", models.FloatField()),
                ("traceability", models.BooleanField()),
                (
                    "defaultMedium",
                    models.CharField(
                        choices=[
                            ("article", "Article"),
                            ("fabric", "Tissu"),
                            ("yarn", "Fil"),
                        ],
                        max_length=50,
                    ),
                ),
                ("pcrWaste", models.FloatField()),
                (
                    "complexity",
                    models.CharField(
                        choices=[
                            ("very-high", "Très élevée"),
                            ("high", "Élevée"),
                            ("medium", "Moyenne"),
                            ("low", "Faible"),
                            ("very-low", "Très faible"),
                            ("not-applicable", "Non applicable"),
                        ],
                        max_length=50,
                    ),
                ),
                ("durationInMinutes", models.FloatField()),
                ("daysOfWear", models.IntegerField()),
                ("defaultNbCycles", models.IntegerField()),
                ("ratioDryer", models.FloatField()),
                ("ratioIroning", models.FloatField()),
                ("timeIroning", models.FloatField()),
                ("wearsPerCycle", models.FloatField()),
                ("volume", models.FloatField()),
                (
                    "ironingProcessUuid",
                    models.ForeignKey(
                        null=True,
                        on_delete=django.db.models.deletion.SET_NULL,
                        related_name="productsIroning",
                        to="textile.process",
                    ),
                ),
                (
                    "nonIroningProcessUuid",
                    models.ForeignKey(
                        null=True,
                        on_delete=django.db.models.deletion.SET_NULL,
                        related_name="productsNonIroning",
                        to="textile.process",
                    ),
                ),
            ],
        ),
        migrations.CreateModel(
            name="Example",
            fields=[
                (
                    "id",
                    models.CharField(max_length=50, primary_key=True, serialize=False),
                ),
                ("name", models.CharField(max_length=200)),
                (
                    "category",
                    models.CharField(
                        choices=[
                            ("Chemise", "Chemise"),
                            ("Jean", "Jean"),
                            ("Jupe / Robe", "Jupe / Robe"),
                            ("Manteau / Veste", "Manteau / Veste"),
                            ("Pantalon / Short", "Pantalon / Short"),
                            (
                                "Pull / Couche intermédiaire",
                                "Pull / Couche intermédiaire",
                            ),
                            ("Tshirt / Polo", "Tshirt / Polo"),
                        ],
                        max_length=50,
                    ),
                ),
                ("mass", models.FloatField()),
                (
                    "business",
                    models.CharField(
                        choices=[
                            ("small-business", "PME/TPE"),
                            (
                                "large-business-with-services",
                                "Grande entreprise proposant un service de réparation et de garantie",
                            ),
                            (
                                "large-business-without-services",
                                "Grande entreprise ne proposant pas de service de réparation ou de garantie",
                            ),
                        ],
                        max_length=50,
                    ),
                ),
                ("marketingDuration", models.FloatField(null=True)),
                ("numberOfReferences", models.IntegerField(null=True)),
                ("price", models.FloatField(null=True)),
                ("repairCost", models.FloatField(blank=True, null=True)),
                ("traceability", models.BooleanField(null=True)),
                (
                    "countrySpinning",
                    models.CharField(
                        choices=[
                            ("---", "Inconnu (par défaut)"),
                            ("REO", "Région - Europe de l'Ouest"),
                            ("REE", "Région - Europe de l'Est"),
                            ("RAS", "Région - Asie"),
                            ("RAF", "Région - Afrique"),
                            ("RMO", "Région - Moyen-Orient"),
                            ("RAL", "Région - Amérique Latine"),
                            ("RAN", "Région - Amérique du nord"),
                            ("ROC", "Région - Océanie"),
                            ("AU", "Australie"),
                            ("AL", "Albanie"),
                            ("BD", "Bangladesh"),
                            ("BE", "Belgique"),
                            ("BR", "Brésil"),
                            ("CH", "Suisse"),
                            ("CN", "Chine"),
                            ("CZ", "Tchèquie"),
                            ("DE", "Allemagne"),
                            ("EG", "Egypte"),
                            ("ES", "Espagne"),
                            ("ET", "Ethiopie"),
                            ("FR", "France"),
                            ("GB", "Royaume-Uni"),
                            ("GR", "Grèce"),
                            ("HU", "Hongrie"),
                            ("IN", "Inde"),
                            ("IT", "Italie"),
                            ("KE", "Kenya"),
                            ("KH", "Cambodge"),
                            ("MA", "Maroc"),
                            ("MM", "Myanmar"),
                            ("NL", "Pays-Bas"),
                            ("NZ", "Nouvelle-Zélande"),
                            ("PE", "Pérou"),
                            ("PK", "Pakistan"),
                            ("PL", "Pologne"),
                            ("PT", "Portugal"),
                            ("RO", "Roumanie"),
                            ("LK", "Sri Lanka"),
                            ("TN", "Tunisie"),
                            ("TR", "Turquie"),
                            ("TW", "Taiwan"),
                            ("US", "Etats-Unis"),
                            ("VN", "Vietnam"),
                        ],
                        max_length=50,
                    ),
                ),
                (
                    "countryFabric",
                    models.CharField(
                        choices=[
                            ("---", "Inconnu (par défaut)"),
                            ("REO", "Région - Europe de l'Ouest"),
                            ("REE", "Région - Europe de l'Est"),
                            ("RAS", "Région - Asie"),
                            ("RAF", "Région - Afrique"),
                            ("RMO", "Région - Moyen-Orient"),
                            ("RAL", "Région - Amérique Latine"),
                            ("RAN", "Région - Amérique du nord"),
                            ("ROC", "Région - Océanie"),
                            ("AU", "Australie"),
                            ("AL", "Albanie"),
                            ("BD", "Bangladesh"),
                            ("BE", "Belgique"),
                            ("BR", "Brésil"),
                            ("CH", "Suisse"),
                            ("CN", "Chine"),
                            ("CZ", "Tchèquie"),
                            ("DE", "Allemagne"),
                            ("EG", "Egypte"),
                            ("ES", "Espagne"),
                            ("ET", "Ethiopie"),
                            ("FR", "France"),
                            ("GB", "Royaume-Uni"),
                            ("GR", "Grèce"),
                            ("HU", "Hongrie"),
                            ("IN", "Inde"),
                            ("IT", "Italie"),
                            ("KE", "Kenya"),
                            ("KH", "Cambodge"),
                            ("MA", "Maroc"),
                            ("MM", "Myanmar"),
                            ("NL", "Pays-Bas"),
                            ("NZ", "Nouvelle-Zélande"),
                            ("PE", "Pérou"),
                            ("PK", "Pakistan"),
                            ("PL", "Pologne"),
                            ("PT", "Portugal"),
                            ("RO", "Roumanie"),
                            ("LK", "Sri Lanka"),
                            ("TN", "Tunisie"),
                            ("TR", "Turquie"),
                            ("TW", "Taiwan"),
                            ("US", "Etats-Unis"),
                            ("VN", "Vietnam"),
                        ],
                        max_length=50,
                    ),
                ),
                (
                    "countryDyeing",
                    models.CharField(
                        choices=[
                            ("---", "Inconnu (par défaut)"),
                            ("REO", "Région - Europe de l'Ouest"),
                            ("REE", "Région - Europe de l'Est"),
                            ("RAS", "Région - Asie"),
                            ("RAF", "Région - Afrique"),
                            ("RMO", "Région - Moyen-Orient"),
                            ("RAL", "Région - Amérique Latine"),
                            ("RAN", "Région - Amérique du nord"),
                            ("ROC", "Région - Océanie"),
                            ("AU", "Australie"),
                            ("AL", "Albanie"),
                            ("BD", "Bangladesh"),
                            ("BE", "Belgique"),
                            ("BR", "Brésil"),
                            ("CH", "Suisse"),
                            ("CN", "Chine"),
                            ("CZ", "Tchèquie"),
                            ("DE", "Allemagne"),
                            ("EG", "Egypte"),
                            ("ES", "Espagne"),
                            ("ET", "Ethiopie"),
                            ("FR", "France"),
                            ("GB", "Royaume-Uni"),
                            ("GR", "Grèce"),
                            ("HU", "Hongrie"),
                            ("IN", "Inde"),
                            ("IT", "Italie"),
                            ("KE", "Kenya"),
                            ("KH", "Cambodge"),
                            ("MA", "Maroc"),
                            ("MM", "Myanmar"),
                            ("NL", "Pays-Bas"),
                            ("NZ", "Nouvelle-Zélande"),
                            ("PE", "Pérou"),
                            ("PK", "Pakistan"),
                            ("PL", "Pologne"),
                            ("PT", "Portugal"),
                            ("RO", "Roumanie"),
                            ("LK", "Sri Lanka"),
                            ("TN", "Tunisie"),
                            ("TR", "Turquie"),
                            ("TW", "Taiwan"),
                            ("US", "Etats-Unis"),
                            ("VN", "Vietnam"),
                        ],
                        max_length=50,
                    ),
                ),
                (
                    "countryMaking",
                    models.CharField(
                        choices=[
                            ("---", "Inconnu (par défaut)"),
                            ("REO", "Région - Europe de l'Ouest"),
                            ("REE", "Région - Europe de l'Est"),
                            ("RAS", "Région - Asie"),
                            ("RAF", "Région - Afrique"),
                            ("RMO", "Région - Moyen-Orient"),
                            ("RAL", "Région - Amérique Latine"),
                            ("RAN", "Région - Amérique du nord"),
                            ("ROC", "Région - Océanie"),
                            ("AU", "Australie"),
                            ("AL", "Albanie"),
                            ("BD", "Bangladesh"),
                            ("BE", "Belgique"),
                            ("BR", "Brésil"),
                            ("CH", "Suisse"),
                            ("CN", "Chine"),
                            ("CZ", "Tchèquie"),
                            ("DE", "Allemagne"),
                            ("EG", "Egypte"),
                            ("ES", "Espagne"),
                            ("ET", "Ethiopie"),
                            ("FR", "France"),
                            ("GB", "Royaume-Uni"),
                            ("GR", "Grèce"),
                            ("HU", "Hongrie"),
                            ("IN", "Inde"),
                            ("IT", "Italie"),
                            ("KE", "Kenya"),
                            ("KH", "Cambodge"),
                            ("MA", "Maroc"),
                            ("MM", "Myanmar"),
                            ("NL", "Pays-Bas"),
                            ("NZ", "Nouvelle-Zélande"),
                            ("PE", "Pérou"),
                            ("PK", "Pakistan"),
                            ("PL", "Pologne"),
                            ("PT", "Portugal"),
                            ("RO", "Roumanie"),
                            ("LK", "Sri Lanka"),
                            ("TN", "Tunisie"),
                            ("TR", "Turquie"),
                            ("TW", "Taiwan"),
                            ("US", "Etats-Unis"),
                            ("VN", "Vietnam"),
                        ],
                        max_length=50,
                    ),
                ),
                (
                    "fabricProcess",
                    models.ForeignKey(
                        null=True,
                        on_delete=django.db.models.deletion.CASCADE,
                        to="textile.process",
                    ),
                ),
                (
                    "product",
                    models.ForeignKey(
                        null=True,
                        on_delete=django.db.models.deletion.CASCADE,
                        to="textile.product",
                    ),
                ),
            ],
        ),
        migrations.CreateModel(
            name="Share",
            fields=[
                (
                    "id",
                    models.BigAutoField(
                        auto_created=True,
                        primary_key=True,
                        serialize=False,
                        verbose_name="ID",
                    ),
                ),
                ("share", models.FloatField()),
                (
                    "example",
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        to="textile.example",
                    ),
                ),
                (
                    "material",
                    models.ForeignKey(
                        on_delete=django.db.models.deletion.CASCADE,
                        to="textile.material",
                    ),
                ),
            ],
        ),
        migrations.AddField(
            model_name="example",
            name="materials",
            field=models.ManyToManyField(
                related_name="examples", through="textile.Share", to="textile.material"
            ),
        ),
    ]