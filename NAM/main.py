from nam.config.default import defaults
from nam.utils import *
from nam.config import defaults
from nam.models import NAM
from nam.utils import plot_mean_feature_importance
from nam.utils import plot_nams
from sklearn.preprocessing import StandardScaler



if __name__ == "__main__":
    config = defaults()
    config.activation = "Relu"
    config.decay_rate = 0.05
    from nam.data.datasets import load_UKload


    dataset = load_UKload(config, "UKL.csv")

    dataloaders = dataset.train_dataloaders()
    train_loader, val_loader = dataset.train_dl, dataset.val_dl

    config.num_epochs = 500
    config.weight_decay = 0
    config.decay_rate = 0.0
    config.dropout=0.0
    config.l2_regularization=0
    config.actiavtion = "exu"
    config.hidden_sizes = [100, 100]
    config.output_regularization = 0
    
    model = NAM(
      config=config,
      name="uk",
      num_inputs=7,
      num_units=1000,
    )

    from nam.trainer.trainer import Trainer as NAMTrainer

    trainer = NAMTrainer(config=config, model=model, dataset=dataset, loss="mae")

    # Train the model
    trainer.train()

    # Test the model
    metric = trainer.test()
    print(metric)

    fig = plot_mean_feature_importance(model, dataset)
    fig = plot_nams(model, dataset, num_cols=7)


    # Test the model and generate predictions
    test_loader = dataset.test_dl
    predictions = []
    actuals = []
    for batch in test_loader:
        features, target = batch
        output, _ = model(features)
        predictions.append(output.detach().numpy())
        actuals.append(target.detach().numpy())

    predictions = np.concatenate(predictions, axis=0)
    actuals = np.concatenate(actuals, axis=0)

    # Convert predictions and actuals to 1D arrays
    predictions = predictions.flatten()
    actuals = actuals.flatten()

    # Plot predicted vs actual values
    fig, ax = plt.subplots(figsize=(10, 8), frameon=False)
    ax.scatter(actuals, predictions, c='darkred')
    ax.set_xlabel("Actual Values", fontsize=16)
    ax.set_ylabel("Predicted Values", fontsize=16)
    ax.set_title("Predicted vs Actual Values", fontsize=18)

    # Add a regression line
    p = np.polyfit(actuals, predictions, 1)
    ax.plot(actuals, np.polyval(p, actuals), linestyle="--", linewidth=2, c='black')

    # Set the background color and grid style
    # ax.set_facecolor('#F2F2F2')
    # ax.grid(color='white', linestyle='-', linewidth=1)

    plt.show()





    # # Generate predictions and targets
    # predictions = []
    # targets = []
    #
    # for batch in trainer.dataloader_test:
    #     features, target = batch
    #     output, _ = trainer.model(features)
    #     predictions.append(output.detach().numpy())
    #     targets.append(target.detach().numpy())
    #
    # predictions = np.concatenate(predictions, axis=0)
    # targets = np.concatenate(targets, axis=0)
    #
    # # Plot the targets and predictions
    # plt.plot(targets, 'o', color='BLACK', label='Targets',  markersize=6)
    # plt.plot(predictions, 'o', color='darkred', label='Predictions',  markersize=6)
    # plt.xlabel("Data points", fontsize='x-large')
    # plt.ylabel("Values", fontsize='x-large')
    # # plt.title("Targets and Predictions", fontsize='x-large')
    # plt.legend()
    # plt.show()