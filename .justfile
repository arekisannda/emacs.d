set default-list := true

[doc('Run all checks')]
check-all: lint manual

[doc('Run lint checks')]
lint:
    @pre-commit run --all-files

[doc('Run manual checks')]
manual:
    @pre-commit run --all-files -c .pre-commit-manual.yaml
