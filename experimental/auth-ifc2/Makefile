SOUFFLE = souffle
SOUFFLE_ARGS = ''

module:
	$(SOUFFLE) moduleCheckExample.dl && \
		echo "flowsTo:" && cat flowsTo.csv && \
		echo "bindModule:" && cat bindModule.csv && \
		echo "labelEnv:" && cat labelEnvironment.csv && \
		echo "saysDowngrades:" && \
			echo "old_label/speaker  in_particle  location  new_label:" && \
			cat saysDowngrades.csv && \
		echo "moduleLabelChecks:" && cat moduleLabelChecks.csv

edge:
	$(SOUFFLE) edge_example.dl && \
		echo "flowsTo:" && cat flowsTo.csv && \
		echo "bindModule:" && cat bindModule.csv && \
		echo "labelEnv:" && cat labelEnvironment.csv && \
		echo "saysDowngrades:" && \
			echo "old_label/speaker  in_particle  location  new_label:" && \
			cat saysDowngrades.csv && \
		echo "moduleLabelChecks:" && cat moduleLabelChecks.csv

clean:
	rm *csv
