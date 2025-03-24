# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of Kala
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Format and check code ---------------------------------------------------
styler::style_pkg()
OhdsiRTools::checkUsagePackage("Kala")
OhdsiRTools::updateCopyrightYearFolder()

# Devtools check -----------------------------------------------------------
devtools::spell_check()
devtools::check()

# Create manual -----------------------------------------------------------
unlink("extras/Kala.pdf")
shell("R CMD Rd2pdf ./ --output=extras/Kala.pdf")

# Build site---------------------------------------------------------
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Release package to CRAN ------------------------------------------------------
devtools::release()
devtools::check(cran=TRUE)

