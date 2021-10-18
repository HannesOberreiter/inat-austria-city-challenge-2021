# Description -------------------------------------------------------------
# Catcher for our Modules
print("Loaded Functions")

# Load Modules ------------------------------------------------------------
for (f in list.files(path = glue("{here()}/functions/modules/"), pattern = "*.R")) {
    source(glue("{here()}/functions/modules/{f}"))
}
rm(f)
print("Functions Loaded")