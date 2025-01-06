# import matplotlib.pyplot as plt
# import numpy as np
#
# from nam.data.datasets import load_UKload
# from nam.config.default import defaults
# from nam.models import NAM
#
# if __name__ == "__main__":
#     # Load dataset and configuration
#     config = defaults()
#     dataset = load_UKload(config, "uk.csv")
#
#     # Initialize and load trained NAM model
#     model = NAM(config=config, name="uk", num_inputs=7, num_units=1000)
#
#
#     # Get predictions and true values
#     preds = model.predict(dataset.x_test)
#     true = dataset.y_test
#
#     # Plot predictions versus true values
#     plt.plot(true, label="True")
#     plt.plot(preds, label="Predicted")
#     plt.legend()
#     plt.show()
